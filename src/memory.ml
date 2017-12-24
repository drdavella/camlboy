open Core
open Types


let source_dest_str dest source =
  let dest_str = reg_to_str dest in
  let source_str = reg_to_str source in
  sprintf "%s <- %s" dest_str source_str

let dest_index high low =
  (high / low) * (high - 0x4)

let load_reg_from_reg dest source state =
  Utils.set_register state dest (Utils.get_register state source);
  4

let load_register high low state =
  let source_reg = register_array.(low) in
  let dest_reg = register_array.(dest_index high low) in
  let tick = match dest_reg, source_reg with
  | HL, _ -> raise (NotImplemented "LD (HL), x is not yet implemented")
  | _, HL -> raise (NotImplemented "LD x, (HL) is not yet implemented")
  | Reg dest, Reg source -> load_reg_from_reg dest source state
  in
  let msg = (sprintf "LD ") ^ (source_dest_str dest_reg source_reg) in
  (Utils.increment_pc state 1), tick, msg
