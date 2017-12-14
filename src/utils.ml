open Types
open Unsigned

let pack_ints_to_u16 high low =
  let word = UInt16.of_int high in
  UInt16.logor (UInt16.shift_left word 8) (UInt16.of_int low)

let increment_pc state increment =
  state.pc <- UInt16.add state.pc (UInt16.of_int increment);
  state
