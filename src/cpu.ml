open Core

let rom_start_index = 0x100
exception UnrecognizedOpcode of string

let unknown_opcode opcode pc =
  let msg = sprintf "unknown opcode 0x%02x at pc 0x%04x" opcode pc in
  raise (UnrecognizedOpcode msg)

let decode opcode pc =
  let%bitstring bits = {|opcode : 8|} in
  match%bitstring bits with
  | {| 0x00 : 8 |} -> printf "hey it's 0x00\n"
  | {| _ |} -> unknown_opcode opcode pc

let rec run_loop rom_array pc rom_size =
  decode (rom_array.(pc)) pc;
  run_loop rom_array (pc + 1) rom_size

let emulate rom_array =
  (* TODO: eventually it shouldn't be necessary to check the array size *)
  let rom_size = Array.length rom_array in
  run_loop rom_array rom_start_index rom_size
