(*type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

effect Writev : Unix.file_descr * (buffer * int * int) list -> int

let writev fd iovecs = perform (Writev (fd, iovecs))

external stub_writev : Unix.file_descr -> (buffer * int * int) list -> int = "aeio_unix_bytes_writev"

*)
type 'a _promise =
    Waiting of ('a,unit) continuation list
  | Done of 'a

  type 'a promise = 'a _promise ref

  effect Async : (unit -> 'a) -> 'a promise
  let async f = perform (Async f)

  effect Yield : unit
  let yield () = perform Yield

  effect Await : 'a promise -> 'a
  let await p = perform (Await p)

  let q = Queue.create ()
  let enqueue t = Queue.push t q
  let dequeue () =
    if Queue.is_empty q then ()
    else Queue.pop q ()

  let run main =
    let _ = print_endline "Inside new scheduler :-)" in
    let rec fork : 'a. 'a promise -> (unit -> 'a) -> unit =
      fun pr main ->
        match main () with
        | v ->
            let l = match !pr with
              | Waiting l -> l
              | _ -> failwith "impossible"
            in
            List.iter (fun k -> enqueue (fun () -> continue k v)) l;
            pr := Done v;
            dequeue ()
        | effect (Async f) k ->
            let pr = ref (Waiting []) in
            enqueue (fun () -> continue k pr);
            fork pr f
        | effect Yield k ->
            enqueue (continue k);
            dequeue ()
        | effect (Await p) k ->
            begin match !p with
            | Done v -> continue k v
            | Waiting l -> begin
                p := Waiting (k::l);
                dequeue ()
              end
            end
    in
    fork (ref (Waiting [])) main


(* From lwt/src/unix/lwt_main.ml *)
(*let rec run t =
  (* Wakeup paused threads now. *)
  Lwt.wakeup_paused ();
  match Lwt.poll t with
  | Some x -> x
  | None ->
    (* Call enter hooks. *)
    Mirage_runtime.run_enter_iter_hooks ();
    (* Do the main loop call. *)
    Lwt_engine.iter (Lwt.paused_count () = 0);
    (* Wakeup paused threads again. *)
    Lwt.wakeup_paused ();
    (* Call leave hooks. *)
    Mirage_runtime.run_leave_iter_hooks ();
    run t

(* If the platform doesn't have SIGPIPE, then Sys.set_signal will
   raise an Invalid_argument exception. If the signal does not exist
   then we don't need to ignore it, so it's safe to continue. *)
let ignore_sigpipe () =
  try Sys.(set_signal sigpipe Signal_ignore) with Invalid_argument _ -> ()

(* Main runloop, which registers a callback so it can be invoked
   when timeouts expire. Thus, the program may only call this function
   once and once only. *)
let run t =
  ignore_sigpipe ();
  run t

let () =
  at_exit (fun () ->
      Lwt.abandon_wakeups ();
      run (Mirage_runtime.run_exit_hooks ())) *)
