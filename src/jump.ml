open Core

let uncond_imm code_bytes state =
  let msg = sprintf "0x%02x" code_bytes.(0) in
  (Utils.increment_pc state 1), 2, msg
