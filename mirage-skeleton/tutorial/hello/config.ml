open Mirage

let main =
  foreign
    ~packages:[package "duration"]
    "Unikernel.Hello" (time @-> pclock @-> mclock @-> job)

let () =
  register "hello" [main $ default_time $ default_posix_clock $ default_monotonic_clock]
