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

let load_compound_imm dest high low state =
  let high_reg, low_reg = match dest with
  | BC -> B, C
  | DE -> D, E
  | HL -> H, L
  in
  Utils.set_register state high_reg (UInt8.of_int high);
  Utils.set_register state low_reg (UInt8.of_int low)

let load_imm_d high code_bytes state =
  let dest = compound_array.(high) in
  let high, low = code_bytes.(1), code_bytes.(0) in
  let () = match dest with
  | Double dest_reg -> load_compound_imm dest_reg high low state
  | SP -> state.sp <- Utils.pack_ints_to_u16 high low
  in
  let msg = sprintf "LD IMM %s <- 0x%02x%02x" (comp_to_str dest) high low in
  (Utils.increment_pc state 3), 12, msg

let load_imm high low code_bytes state =
  let imm_val = code_bytes.(0) in
  let dest_reg = register_array.((low / 0x8) + (high * 2) + 1) in
  let tick = match dest_reg with
  | Reg dest -> Utils.set_register state dest (UInt8.of_int imm_val); 8
  | HL -> raise (NotImplemented "LD IMM (HL) is not yet implemented")
  in
  let msg = sprintf "LD IMM %s <- 0x%02x" (reg_to_str dest_reg) imm_val in
  (Utils.increment_pc state 2), tick, msg
