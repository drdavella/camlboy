open Core
open Types
open Unsigned

let uncond_imm code_bytes state =
  let addr = Utils.pack_ints_to_u16 code_bytes.(1) code_bytes.(0) in
  let msg = sprintf "JUMP IMM TO 0x%04x" (UInt16.to_int addr) in
  state.pc <- addr;
  state , 16, msg
