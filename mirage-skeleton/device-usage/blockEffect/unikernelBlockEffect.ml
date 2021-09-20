open Lwt.Infix
open Printf

module Main (Time: Mirage_time.S)  (PClock: Mirage_clock.PCLOCK) (MClock: Mirage_clock.MCLOCK) (B: Mirage_block.S) = struct
  let log_src = Logs.Src.create "block" ~doc:"block tester"
  module Log = (val Logs.src_log log_src : Logs.LOG)


  let fill_with_pattern x phrase =
    for i = 0 to Cstruct.length x - 1 do
      Cstruct.set_char x i phrase.[i mod (String.length phrase)]
    done

  let fill_with_zeroes x =
    for i = 0 to Cstruct.length x - 1 do
      Cstruct.set_uint8 x i 0
    done

  let cstruct_equal a b =
    let check_contents a b =
      try
        for i = 0 to Cstruct.length a - 1 do
          let a' = Cstruct.get_char a i in
          let b' = Cstruct.get_char b i in
          if a' <> b' then raise Not_found (* won't escape *)
        done;
        true
      with _ -> false in
    (Cstruct.length a = (Cstruct.length b)) && (check_contents a b)

  let check_equal a b =
    if not(cstruct_equal a b) then begin
      Log.warn (fun f -> f "Buffers unequal: %S vs %S"
        (Cstruct.to_string a) (Cstruct.to_string b))
    end

  let alloc sector_size n =
    let rec loop = function
      | 0 -> []
      | n ->
        let page = Io_page.(to_cstruct (get 1)) in
        let phrase = sprintf "%d: All work and no play makes Dave a dull boy.\n" n in
        let sector = Cstruct.sub page 0 sector_size in
        fill_with_pattern sector phrase;
        sector :: (loop (n-1)) in
    loop n

  open Mirage_block

  let check_sector_write b _kind _id offset length =
    Log.info (fun f -> f "writing %d sectors at %Ld\n" length offset);

(*    let info = (B.get_info b) in --------> TODO: get_info is (t -> info Lwt.t)*)
   let sectors = alloc (*info.sector_size*)512 length in
    (try (B.write b offset sectors) with
       | e -> Log.info (fun f -> f "\nSome Error happened while writing %s" (Printexc.to_string e)) 
    );
    (* Unix.sleep 5; *)
    (* let sectors = alloc info.sector_size512 length in *)
    let sectors' = alloc 512 length in
    List.iter fill_with_zeroes sectors';
    (try (B.read b offset sectors' ) with
       | e -> Log.info (fun f -> f "\nSome Error happened while reading %s" (Printexc.to_string e))) ;
    
    List.iter (fun (a, b) -> check_equal a b) (List.combine sectors sectors') 



 let str_of_time (posix_time, timezone) =
    Format.asprintf "%a" (Ptime.pp_human ?tz_offset_s:timezone ()) posix_time

  let start _time pclock mclock b () =
    B.get_info b >>= fun info ->
    (* FIXME(samoht): this should probably move into
       Mirage_block.pp_info *)
    Log.info (fun f -> f "sectors = %Ld\nread_write=%b\nsector_size=%d\n%!"
      info.size_sectors info.read_write info.sector_size);
      
      let current_time = PClock.now_d_ps pclock |> Ptime.v in
      let tz = PClock.current_tz_offset_s pclock in
      let str =
        Printf.sprintf
          "\n First Time %Lu nanoseconds have elapsed. \n\
          \ At the stroke, the time will be %s \x07 *BEEP*"
          (MClock.elapsed_ns mclock) @@ str_of_time (current_time, tz)
          in 
                Logs.info (fun f -> f "%s" str);

    
    check_sector_write b "local" "51712" 0L 100000;

      
    
      let current_time = PClock.now_d_ps pclock |> Ptime.v in
      let tz = PClock.current_tz_offset_s pclock in
      let str =
        Printf.sprintf
          "\n First Time %Lu nanoseconds have elapsed. \n\
          \ At the stroke, the time will be %s \x07 *BEEP*"
          (MClock.elapsed_ns mclock) @@ str_of_time (current_time, tz)
          in 
                Logs.info (fun f -> f "%s" str);
                Lwt.return ()
    

end
