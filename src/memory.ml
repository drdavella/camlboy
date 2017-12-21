open Core
open Types

let source_dest_str dest source =
  sprintf "%s <- %s" (register_to_str dest) (register_to_str source)

let dest_index high low =
  (high / low) * (high - 0x4)

let load_register high low code_bytes state =
  let source_reg = register_array.(low) in
  let dest_reg = register_array.(dest_index high low) in
  let msg = (sprintf "LD ") ^ (source_dest_str dest_reg source_reg) in
  (Utils.increment_pc state 1), 4, msg
