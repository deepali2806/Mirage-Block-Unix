open Mirage

type shellconfig = ShellConfig
let shellconfig = Type ShellConfig

let config_shell = impl @@ object
    inherit base_configurable

    method! build _i =
      Bos.OS.Cmd.run Bos.Cmd.(v "dd" % "if=/dev/zero" % "of=disk.img" % "count=1000000")
(*Count says copy N input blocks. When after make we see the size of disk.img it is 51200000 i.e. 51.2MB. It means each block is of size 512 Bytes*)
    method! clean _i =
      Bos.OS.File.delete (Fpath.v "disk.img")

    method module_name = "Functoria_runtime"
    method name = "shell_config"
    method ty = shellconfig
end


let main =
  let packages = [ package "io-page"; package "duration"; package ~build:true "bos"; package ~build:true "fpath" ] in
  foreign
    ~packages
    ~deps:[abstract config_shell] "UnikernelBlockEffect.Main" (time @-> pclock @-> mclock @-> block @-> job)

let img = Key.(if_impl is_solo5 (block_of_file "storage") (block_of_file "disk.img"))

let () =
  register "block_test" [main $ default_time $ default_posix_clock $ default_monotonic_clock $ img]
