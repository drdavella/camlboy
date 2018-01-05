(* This file is named "flagx" so it doesn't conflict with another module *)
open Core
open Types

type flags =
  | Zero | Subtract | Half | Carry

type flag_conditions =
  | NotZero | Zero | NoCarry | Carry

let flag_array =
  Array.of_list [ NotZero; Zero; NoCarry; Carry ]

let cond_to_str condition =
  match condition with
  | NotZero -> "NZ"
  | Zero -> "Z"
  | NoCarry -> "NC"
  | Carry -> "C"

let set_flag flag state =
  match flag with
  | Subtract -> state.flags.n = true
  | Half -> state.flags.h = true
  | Carry -> state.flags.c = true
  | Zero -> state.flags.z = true

let get_flag flag state =
  match flag with
  | Subtract -> state.flags.n
  | Half -> state.flags.h
  | Carry -> state.flags.c
  | Zero -> state.flags.z

let get_condition condition state =
  match condition with
  | NotZero -> not state.flags.z
  | Zero -> state.flags.z
  | NoCarry -> not state.flags.c
  | Carry -> state.flags.c
