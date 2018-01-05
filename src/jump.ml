open Core
open Types
open Flagx
open Unsigned


let uncond_imm code_bytes state =
  let addr = Utils.pack_ints_to_u16 code_bytes.(1) code_bytes.(0) in
  let msg = sprintf "JUMP IMM TO 0x%04x" (UInt16.to_int addr) in
  state.pc <- addr;
  state, 16, msg

let ubyte_to_sbyte byte =
  -(byte land 0x80) + (byte land 0x7f)

let do_jump offset state =
  (* Account for the size of this instruction in the offset *)
  let new_addr = (UInt16.to_int state.pc) + offset + 2 in
  state.pc <- UInt16.of_int new_addr;
  sprintf "to 0x%04x" new_addr

let cond_imm condition code_bytes state =
  let msg = sprintf "JP %s" (cond_to_str condition) in
  match get_condition condition state with
  (* The offset is treated as an 8-bit signed integer *)
  | true ->
      let new_msg = do_jump (ubyte_to_sbyte code_bytes.(0)) state in
      state, 12, (sprintf "%s %s" msg new_msg)
  | false -> (Utils.increment_pc state 2), 8, sprintf "%s: no jump" msg
