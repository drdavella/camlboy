open Core.Std
open Core_kernel.In_channel


let rec process_rom infile =
  match input_byte infile with
  | Some x -> x :: process_rom infile
  | None -> []

let read_rom filename =
  let rom_list = In_channel.with_file filename ~f:process_rom in
  Array.of_list rom_list

let rec emulate rom_array index =
  printf "rom[%04x]=%02x\n" index rom_array.(index);
  emulate rom_array (index + 1)

let run filename =
  let rom_array = read_rom filename in
  printf "rom file=%s, size=%d\n" filename (Array.length rom_array);
  emulate rom_array 0

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
