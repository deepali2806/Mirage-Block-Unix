open Lwt.Infix

module Hello (Time : Mirage_time.S) (PClock: Mirage_clock.PCLOCK) (MClock: Mirage_clock.MCLOCK) = struct

 let str_of_time (posix_time, timezone) =
    Format.asprintf "%a" (Ptime.pp_human ?tz_offset_s:timezone ()) posix_time

  let start _time pclock mclock =
	let current_time = PClock.now_d_ps pclock |> Ptime.v in
      let tz = PClock.current_tz_offset_s pclock in
      let str =
        Printf.sprintf
          "\n First Time %Lu nanoseconds have elapsed. \n\
          \ At the stroke, the time will be %s \x07 *BEEP*"
          (MClock.elapsed_ns mclock) @@ str_of_time (current_time, tz)
          in 
                Logs.info (fun f -> f "%s" str);
    let rec loop = function
      | 0 -> () (*Lwt.return_unit*)
      | n ->
        Logs.info (fun f -> f "hello");
        Unix.sleep 1;
        (*Time.sleep_ns (Duration.of_sec 1) >>= fun () ->*)
        loop (n-1)
    in
    loop 4;
    let current_time = PClock.now_d_ps pclock |> Ptime.v in
      let tz = PClock.current_tz_offset_s pclock in
      let str =
        Printf.sprintf
          "\n At the end %Lu nanoseconds have elapsed. \n\
          \ At the stroke, the time will be %s \x07 *BEEP*"
          (MClock.elapsed_ns mclock) @@ str_of_time (current_time, tz)
      in
      Logs.info (fun f -> f "%s" str)

end
