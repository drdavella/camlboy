open Core.Std

let run filename =
  printf "rom file=%s\n" filename

let spec =
  let open Command.Spec in
  empty
  +> anon ("rom file" %: string)

let command =
  Command.basic
    ~summary:"Run the GBC emulator"
    spec
    (fun filename () -> run filename)

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
