(*
 * Copyright (C) 2011-2013 Citrix Inc
 * Copyright (C) 2016 Docker Inc
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *)

let src =
  let src = Logs.Src.create "mirage-block-unix" ~doc:"Mirage BLOCK interface for Unix" in
  Logs.Src.set_level src (Some Logs.Info);
  src

(* samoht: `Msg should be the list of all possible exceptions *)
type error = [ Mirage_block.error | `Msg of string ]

(* samoht: `Msg should be the list of all possible exceptions *)
type write_error = [ Mirage_block.write_error | `Msg of string ]

let pp_error ppf = function
  | #Mirage_block.error as e -> Mirage_block.pp_error ppf e
  | `Msg s -> Fmt.string ppf s

let pp_write_error ppf = function
  | #Mirage_block.write_error as e -> Mirage_block.pp_write_error ppf e
  | `Msg s -> Fmt.string ppf s

module Log = (val Logs.src_log src : Logs.LOG)

let is_win32 = Sys.os_type = "Win32"

module Raw = struct
  external openfile_unbuffered: string -> bool -> int -> Unix.file_descr = "stub_openfile_direct"
  let openfile_buffered name rw perm =
    Unix.openfile name [ if rw then Unix.O_RDWR else Unix.O_RDONLY ] perm
	
  external blkgetsize: Unix.file_descr -> int64 = "stub_blkgetsize"
  external blkgetsectorsize: Unix.file_descr -> int = "stub_blkgetsectorsize"

  external lseek_data : Unix.file_descr -> int64 -> int64 = "stub_lseek_data_64"

  external lseek_hole : Unix.file_descr -> int64 -> int64 = "stub_lseek_hole_64"

  type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  external writev_job: Unix.file_descr -> (buffer * int * int) list -> int Lwt_unix.job = "mirage_block_unix_writev_job"
  external readv_job: Unix.file_descr -> (buffer * int * int) list -> int Lwt_unix.job = "mirage_block_unix_readv_job"

  external stub_writev : Unix.file_descr -> (buffer * int * int) list -> int = "aeio_unix_bytes_writev"
  external stub_readv : Unix.file_descr -> (buffer * int * int) list -> int = "aeio_unix_bytes_readv"


  external iov_len: unit -> int = "mirage_block_unix_get_iov_len"

  external chsize_job: Unix.file_descr -> int64 -> unit Lwt_unix.job = "mirage_block_unix_chsize_job"

  external flock: Unix.file_descr -> bool (* ex *) -> bool (* nb *) -> unit   = "stub_flock"
end

let ftruncate fd size =
  if is_win32
  then Lwt_unix.run_job (Raw.chsize_job (Lwt_unix.unix_file_descr fd) size)
  else Lwt_unix.LargeFile.ftruncate fd size

open Lwt

module Config = struct
  type sync_behaviour = [
    | `ToOS
    | `ToDrive
  ]

  let sync_behaviour_of_string = function
    | "0" | "none" -> None
    | "1" | "drive" -> Some `ToDrive
    | "os" -> Some `ToOS
    | _ -> None

  let string_of_sync = function
    | None -> "none"
    | Some `ToDrive -> "drive"
    | Some `ToOS -> "os"

  type t = {
    buffered: bool;
    sync: sync_behaviour option;
    path: string;
    lock: bool;
  }

  let create ?(buffered = true) ?(sync = Some `ToOS) ?(lock = false) path =
    { buffered; sync; path; lock }

  let to_string t =
    let query = [
      "buffered", [ if t.buffered then "1" else "0" ];
      "sync",     [ string_of_sync t.sync ];
      "lock",     [ if t.lock then "1" else "0" ];
    ] in
    let u = Uri.make ~scheme:"file" ~path:t.path ~query () in
    Uri.to_string u

  let of_string x =
    let u = Uri.of_string x in
    match Uri.scheme u with
    | Some "file" ->
      let query = Uri.query u in
      let buffered = try List.assoc "buffered" query = [ "1" ] with Not_found -> false in
      let sync     = try sync_behaviour_of_string @@ List.hd @@ List.assoc "sync" query with Not_found -> None in
      let lock     = try List.assoc "lock" query = [ "1" ] with Not_found -> false in
      let path = Uri.(pct_decode @@ path u) in
      Ok { buffered; sync; path; lock }
    | _ ->
      Error (`Msg "Config.to_string expected a string of the form file://<path>?sync=(none|os|drive)&buffered=(0|1)&lock=(0|1)")
end

type t = {
  mutable fd: (*Lwt_unix*)Unix.file_descr option;
  mutable seek_offset: int64;
  (* a shadow copy of the fd's seek offset which avoids calling `lseek`
     unnecessarily, speeding up sequential read and write *)
  m: Lwt_mutex.t;
  mutable info: Mirage_block.info;
  size_bytes: int64; (* used to handle the last sector, if the file isn't a multiple *)
  config: Config.t;
  use_fsync_after_write: bool;
}

let to_config x = x.config

let (>>*=) m f = m >>= function
  | Ok x -> f x
  | Error x -> Lwt.return (Error x)

exception SomeError of string


let check_descriptor ch =

  if ch < 0 then
    raise (Unix.Unix_error (Unix.EBADF, "check_descriptor", ""))
  else 
  ()
(* Needs Error handling=========================================Error Prone ================================= *)
(* *These functions are more lie pipe in bash : e |> f ---> f e *)
 let stat _filename fd =
  (* try Unix.LargeFile.fstat fd with
  | e -> Log.info (fun f -> f "Error while taking stats of file") *)
  (* check_descriptor fd; *)
  Unix.LargeFile.fstat fd  

let blkgetsize _filename fd =
(* try Raw.blkgetsize fd with
  | e -> Log.info (fun f -> f "Error while getting block size") *)
  Raw.blkgetsize fd 

let blkgetsectorsize fd =
(* try Raw.blkgetsectorsize fd with
  | e -> Log.info (fun f -> f "Error while getting block sector size")  *)
  Raw.blkgetsectorsize fd 

let get_file_size filename fd =
   let st = stat filename fd in
  match st.Unix.LargeFile.st_kind with
  | Unix.S_REG -> st.Unix.LargeFile.st_size
  | Unix.S_BLK -> blkgetsize () fd
  | _ ->
    Log.err (fun f -> f "get_file_size %s: entity is neither a file nor a block device" filename);
   raise
         (SomeError "get_file_size : neither a file nor a block device" ) 




let get_sector_size filename fd =
  let st = stat filename fd in
  match st.Unix.LargeFile.st_kind with
  | Unix.S_REG ->  512 (* FIXME: no easy way to determine this *)
  | Unix.S_BLK ->  blkgetsectorsize fd
  | _ ->
    Log.err (fun f -> f "get_sector_size %s: entity is neither a file nor a block device" filename);
    (* Lwt.return @@ Error
      (`Msg *)
         raise  (SomeError "get_sector_size: neither a file nor a block device" )



let of_config ({ Config.buffered; path; lock; sync = _ } as config) =
  let openfile, use_fsync_after_write = match buffered, is_win32 with
    | true, _ -> Raw.openfile_buffered, false
    | false, false -> Raw.openfile_unbuffered, false
    | false, true ->
      (* We can't use O_DIRECT or F_NOCACHE on Win32, so for now
         we will use `fsync` after every write. *)
      Raw.openfile_buffered, true in
  (* first try read/write and then fall back to read/only *)
  try
    let fd, read_write =
      try
        openfile path true 0o0, true    (**Gives readonly or readwrite while opening file *)
      with _ ->
        openfile path false 0o0, false in
    (* Acquire an exclusive lock if in read/write mode, otherwise a shared lock *)
    if lock then Raw.flock fd read_write true;

    let size_bytes = try get_file_size path fd with 
    | SomeError e -> Unix.close fd;  Log.info (fun f -> f "Error %s" e); -1L
    in 
  
      let  sector_size = try get_sector_size path fd with
          | SomeError e -> Unix.close fd;  Log.info (fun f -> f "Error %s" e); -1
      in 
        (* If the file length is not sector-aligned, we would like to represent the
           last bytes as a sector with zero-padding. Unfortunately on Linux with
           O_DIRECT `read` will fail with EINVAL. *)
        let size_sectors = Int64.(div (add size_bytes (of_int (sector_size-1))) (of_int sector_size)) in
        if Int64.(mul size_sectors (of_int sector_size)) > size_bytes && not(buffered)
        then Log.warn (fun f -> f "Length not sector aligned: O_DIRECT will fail with EINVAL on some platforms");
        (* let fd = Lwt_unix.of_unix_file_descr fd in *)
        let m = Lwt_mutex.create () in
        let seek_offset = 0L in
        { fd = Some fd; seek_offset; m;
                  info = { Mirage_block.sector_size; size_sectors; read_write };
                  size_bytes; config; use_fsync_after_write }
  with _ ->
    Log.err (fun f -> f "connect %s: failed to open file" path);
    raise (SomeError "connect : failed to open file" ) 

(* ======================================================= *)


(* prefix which signals we want to use buffered I/O *)
let buffered_prefix = "buffered:"

let is_prefix ~prefix x =
  let prefix' = String.length prefix and x' = String.length x in
  x' >= prefix' && (String.sub x 0 prefix' = prefix)

let connect ?buffered ?sync ?lock name =
  let legacy_buffered = is_prefix ~prefix:buffered_prefix name in
  (* Keep support for the legacy buffered: prefix until version 3.x.y *)
  let buffered = if legacy_buffered then Some true else buffered in
  let config = Config.create ?buffered ?sync ?lock(*~lock:false*) name in
  of_config config

let disconnect t = match t.fd with
  | Some fd ->
    Unix.close fd;
    t.fd <- None;
    return ()
  | None ->
    return ()

let get_info x = return x.info

let really_read fd = Lwt_cstruct.complete (Lwt_cstruct.read fd)
let really_write fd = Lwt_cstruct.complete (Lwt_cstruct.write fd)


open Mirage_block

let seek_already_locked x fd offset =
  if x.seek_offset <> offset then begin
    x.seek_offset <- offset;
    Unix.LargeFile.lseek (*Lwt_unix.unix_file_descr fd*)fd offset Unix.SEEK_SET
  end else offset

module Cstructs = struct
  (** A list of buffers, like a Unix iovec *)

  let pp_t ppf t =
    List.iter (fun t ->
      Format.fprintf ppf "[%d,%d](%d)" t.Cstruct.off t.Cstruct.len (Bigarray.Array1.dim t.Cstruct.buffer)
    ) t

  let len = List.fold_left (fun acc c -> Cstruct.len c + acc) 0

  let err fmt =
    let b = Buffer.create 20 in                         (* for thread safety. *)
    let ppf = Format.formatter_of_buffer b in
    let k ppf = Format.pp_print_flush ppf (); invalid_arg (Buffer.contents b) in
    Format.kfprintf k ppf fmt

  let rec shift t x =
    if x = 0 then t else match t with
    | [] -> err "Cstructs.shift %a %d" pp_t t x
    | y :: ys ->
      let y' = Cstruct.length y in
      if y' > x
      then Cstruct.shift y x :: ys
      else shift ys (x - y')

  let to_iovec ts =
    List.map (fun t -> t.Cstruct.buffer, t.Cstruct.off, t.Cstruct.len) ts
end

let iov_len = Raw.iov_len ()

let split_list xs maxlen =
  let rec loop (acc, l, n) xs =
    if n = maxlen
    then loop ((List.rev l) :: acc, [], 0) xs
    else match xs with
    | [] -> List.rev ((List.rev l) :: acc)
    | x :: xs -> loop (acc, x :: l, n + 1) xs in
  loop ([], [], 0) xs


let lwt_wrap_exn t op offset ?(buffers=[]) f =
  let fatalf fmt = Printf.ksprintf (fun s ->
      Log.err (fun f -> f "%s" s);
      return (Error (`Msg s))
    ) fmt in
  let describe_buffers buffers =
    if buffers = []
    then ""
    else "with buffers of length [ " ^ (String.concat ", " (List.map (fun b -> string_of_int @@ Cstruct.length b) buffers)) ^ " ]" in
  (* Buffer must be a multiple of sectors in length *)
  Lwt_list.fold_left_s (fun acc b -> match acc with
    | Error e -> Lwt.return (Error e)
    | Ok () ->
      let len = Cstruct.length b in
      if len mod t.info.sector_size <> 0
      then fatalf "%s: buffer length (%d) is not a multiple of sector_size (%d) for file %s" op len t.info.sector_size t.config.Config.path
      else Lwt.return (Ok ())
  ) (Ok ()) buffers 
  >>*= fun () ->
  Lwt.catch f
    (function
      | End_of_file ->
        fatalf "%s: End_of_file at file %s offset %Ld %s" op t.config.Config.path offset (describe_buffers buffers)
      | Unix.Unix_error(code, fn, arg) ->
        fatalf "%s: %s in %s '%s' at file %s offset %Ld %s" op (Unix.error_message code) fn arg t.config.Config.path offset (describe_buffers buffers)
      | e ->
        fatalf "%s: %s at file %s offset %Ld %s" op (Printexc.to_string e) t.config.Config.path offset (describe_buffers buffers)
    )



exception ReadError of string

exception WriteError of string

let wrapException f =
  try f with
    | End_of_file ->  Log.info (fun f -> f "Error opening file")
    | WriteError str -> Log.info (fun f -> f "Error while writing %s" str)
    | ReadError str -> Log.info (fun f -> f "Error while reading %s" str)
    | Unix.Unix_error(code, fn, arg) -> Log.info (fun f -> f "Error while writing %s in %s %s" (Unix.error_message code) fn arg)
    | e -> Log.info (fun f -> f "Unknown Error %s" (Printexc.to_string e)) 
    


let read x sector_start buffers =
  let offset = Int64.(mul sector_start (of_int x.info.sector_size)) in

   Log.info (fun f -> f "Reading Offset %Ld" x.seek_offset);
  wrapException 
    (
      match x.fd with
      | None ->
          raise (ReadError "Disconnected")
      | Some fd ->
        seek_already_locked x fd offset;

        let len = Cstructs.len buffers in
        let len_sectors = (len + x.info.sector_size - 1) / x.info.sector_size in
        if Int64.(add sector_start (of_int len_sectors) > x.info.size_sectors) then begin
          Log.err (fun f -> f "read beyond end of file: sector_start (%Ld) + len (%d) > size_sectors (%Ld)"
                      sector_start len_sectors x.info.size_sectors);
          raise End_of_file
        end 
        else
        begin
                  if is_win32 || List.length buffers = 1 
                    then (* begin
                       let rec loop = function
                        | [] -> Lwt.return_unit
                        | b :: bs ->
                          let virtual_zeroes = Int64.(sub (add offset (of_int (Cstruct.len b))) x.size_bytes) in
                          ( if virtual_zeroes <= 0L
                            then really_read fd b
                            else 
                            begin
                              (* we've had to round up size_sectors to include all the data.
                                 We expect End_of_file but ensure that the data missing from the
                                 file is full of zeroes. *)
                              Cstruct.memset b 0;
                              Lwt.catch
                                (fun () -> really_read fd b)
                                (function
                                  | End_of_file -> Lwt.return_unit
                                  | e -> Lwt.fail e)
                          end );
                          x.seek_offset <- Int64.(add x.seek_offset (of_int (Cstruct.len b)));
                          loop bs in
                      loop buffers 
                    end **) ()
                    else 
                    begin

    (* x.seek_offset <- offset;
    Unix.LargeFile.lseek (Lwt_unix.unix_file_descr fd) offset Unix.SEEK_SET; *)
                      List.iter (fun buffers ->
                        let rec loop remaining =
                          if Cstructs.len remaining = 0 then ()
                          else begin
                            let iovec = Cstructs.to_iovec remaining in
                            let n =  Raw.stub_readv (*Lwt_unix.unix_file_descr fd*)fd iovec in
                            loop (Cstructs.shift remaining n)
                          end in
                        loop buffers
                      ) (split_list buffers iov_len);
                      x.seek_offset <- Int64.add x.seek_offset (Int64.of_int len)
                    end 
                                            
        end
    )






let write x sector_start buffers =
  let offset = Int64.(mul sector_start (of_int x.info.sector_size)) in
  wrapException ( 
      match x with
      | { fd = None; _ } ->
        raise (WriteError "Disconnected")
      | { info = { read_write = false; _ }; _ } ->
        raise (WriteError "Is_read_only")
      | { fd = Some fd; _ } ->
              seek_already_locked x fd offset;

        let len = Cstructs.len buffers in
        let len_sectors = (len + x.info.sector_size - 1) / x.info.sector_size in
        if Int64.(add sector_start (of_int len_sectors) > x.info.size_sectors) 
        then 
        begin
          Log.err (fun f -> f "write beyond end of file: sector_start (%Ld) + len (%d) > size_sectors (%Ld)"
                      sector_start len_sectors x.info.size_sectors);
          raise ( End_of_file ) 
        end 
        else 
          begin
                    if is_win32 || List.length buffers = 1 
                    then begin
                        let rec loop = function
                        | [] -> ()
                        | b :: bs ->
                        (* TODO: If we use write fd b function, wont it thrw error if there is an error with ?file descriptor *)
                          (* Lwt_cstruct.complete (Lwt_cstruct.write fd) b ; *)
                          x.seek_offset <- Int64.(add x.seek_offset (of_int (Cstruct.len b)));
                          loop bs in
                      loop buffers 
                       (* raise ( End_of_file); *)
                    end 
                    else  
                    begin
                      List.iter (fun buffers ->
                        let rec loop remaining =
                          if Cstructs.len remaining = 0 then ()(*Lwt.return_unit *)
                          else 
                          begin
                            let iovec = Cstructs.to_iovec remaining in
                            (* let n = Mirage_unix.writev fd iovec in  *)
                            let n =  Raw.stub_writev fd iovec in
                            loop (Cstructs.shift remaining n)(**Here n is number of bytes written *)
                          end in
                        loop buffers
                      ) (split_list buffers iov_len);
                      (* TODO : Here we can add some try with block to make x.seek offset to point to -1 
                       (fun e -> (**Error case *)
                  x.seek_offset <- -1L; (* actual file pointer is undefined now *)
                  Lwt.fail e;
                ) *)
                      x.seek_offset <- Int64.add x.seek_offset (Int64.of_int len);
                      ( if x.use_fsync_after_write then Unix.fsync (*Lwt_unix.unix_file_descr fd**)fd  else  ());
                      (* Unix.close (Lwt_unix.unix_file_descr fd) *)
                         Log.info (fun f -> f "Writng Offset %Ld" x.seek_offset)

                    end                    
          end
      )
    
(*
let resize t new_size_sectors =
  let new_size_bytes = Int64.(mul new_size_sectors (of_int t.info.sector_size)) in
  match t.fd with
  | None -> return (Error `Disconnected)
  | Some fd ->
    lwt_wrap_exn t "ftruncate" new_size_bytes
        (fun () ->
           Lwt_mutex.with_lock t.m
             (fun () ->
                ftruncate fd new_size_bytes
                >>= fun () ->
                t.info <- { t.info with size_sectors = new_size_sectors };
                return (Ok ())
             )
        )

external flush_job: Unix.file_descr -> bool -> unit Lwt_unix.job = "mirage_block_unix_flush_job"

let flush t =
  match t.fd with
  | None -> return (Error `Disconnected)
  | Some fd ->
    lwt_wrap_exn t "fsync" 0L
      (fun () ->
         ( match t.config.Config.sync with
           | None -> Lwt.return_unit
           | Some `ToOS -> Lwt_unix.run_job (flush_job (Lwt_unix.unix_file_descr fd) false)
           | Some `ToDrive -> Lwt_unix.run_job (flush_job (Lwt_unix.unix_file_descr fd) true)
         )
         >>= fun () ->
         return (Ok ())
      )

let seek_mapped t from =
  match t.fd with
  | None -> return (Error `Disconnected)
  | Some fd ->
    let offset = Int64.(mul from (of_int t.info.sector_size)) in
    lwt_wrap_exn t "seek_mapped" offset
      (fun () ->
         Lwt_mutex.with_lock t.m
           (fun () ->
              let fd = Lwt_unix.unix_file_descr fd in
              let offset = Raw.lseek_data fd offset in
              t.seek_offset <- offset;
              return (Ok Int64.(div offset (of_int t.info.sector_size)))
           )
      )

let seek_unmapped t from =
  match t.fd with
  | None -> return (Error `Disconnected)
  | Some fd ->
    let offset = Int64.(mul from (of_int t.info.sector_size)) in
    lwt_wrap_exn t "seek_unmapped" offset
      (fun () ->
         Lwt_mutex.with_lock t.m
           (fun () ->
              let fd = Lwt_unix.unix_file_descr fd in
              let offset = Raw.lseek_hole fd offset in
              t.seek_offset <- offset;
              return (Ok Int64.(div offset (of_int t.info.sector_size)))
           )
      )

external discard_job: Unix.file_descr -> int64 -> int64 -> unit Lwt_unix.job = "mirage_block_unix_discard_job"

let discard t sector n =
  match t with
  | { fd = None; _ } -> return (Error `Disconnected)
  | { info = { read_write = false; _ }; _ } -> return (Error `Is_read_only)
  | { fd = Some fd; _ } ->
    if is_win32
    then return (Error `Unimplemented)
    else if n = 0L then Lwt.return (Ok ())
    else lwt_wrap_exn t "discard" sector
      (fun () ->
        let fd = Lwt_unix.unix_file_descr fd in
        let offset = Int64.(mul sector (of_int t.info.sector_size)) in
        let n = Int64.(mul n (of_int t.info.sector_size)) in
        Lwt_unix.run_job (discard_job fd offset n)
        >>= fun () ->
        Lwt.return (Ok ())
      )
*)