open Core.Std
open Core_kernel.In_channel


let rec process_rom infile =
  let byte = match input_byte infile with
  | Some x -> [x] :: process_rom infile
  | None -> []
  in
  byte

let read_rom filename =
  let rom_list = In_channel.with_file filename ~f:process_rom in
  Array.of_list rom_list

let emulate rom =
  printf "running program\n"

let run filename =
  let rom_array = read_rom filename in
  let () = printf "rom file=%s, size=%d\n" filename (Array.length rom_array) in
  emulate rom_array

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
