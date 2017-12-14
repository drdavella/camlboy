open Core
open Core_kernel.In_channel


let rec process_rom infile =
  match input_byte infile with
  | Some x -> x :: process_rom infile
  | None -> []

let read_rom filename =
  let rom_list = In_channel.with_file filename ~f:process_rom in
  Array.of_list rom_list

let run filename debug =
  let rom_array = read_rom filename in
  Cpu.emulate rom_array debug

let spec =
  let open Command.Spec in
  empty
  +> flag "-d" no_arg ~doc:" run with debug output"
  +> anon ("rom file" %: string)

let command =
  Command.basic
    ~summary:"Run the GBC emulator"
    spec
    (fun debug filename () -> run filename debug)

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
