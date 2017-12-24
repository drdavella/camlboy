open Core
open Types
open Unsigned


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

let load_imm_d high code_bytes state =
  let dest = compound_array.(high) in
  let imm_val = Utils.pack_ints_to_u16 code_bytes.(1) code_bytes.(0) in
  let () = match dest with
  | SP -> state.sp <- imm_val
  | _ -> raise (NotImplemented "LD whatever is not yet implemented")
  in
  let msg = sprintf "LD IMM %s <- 0x%04x"
    (comp_to_str dest) (UInt16.to_int imm_val) in
  (Utils.increment_pc state 3), 12, msg
