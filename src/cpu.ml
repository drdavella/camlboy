open Core
open Types
open Unsigned

exception UnrecognizedOpcode of string

let unknown_opcode opcode pc =
  let msg = sprintf "unknown opcode 0x%02x at pc 0x%04x" opcode pc in
  raise (UnrecognizedOpcode msg)

let nop state =
  (Utils.increment_pc state 1), 4, "NOP"

let disable_interrupts state =
  (Utils.increment_pc state 1), 4, "DI"

let next_bytes rom_array pc = Array.slice rom_array (pc + 1) 0

let decode opcode rom_array state =
  let pc = UInt16.to_int state.pc in
  let%bitstring bits = {|opcode : 8|} in
  let state, ticks, log_msg =
    match%bitstring bits with
    (* NOP *)
    | {| 0x00 : 8 |} -> nop state
    (* DISABLE INTERRUPTS *)
    | {| 0xf3 : 8 |} -> disable_interrupts state
    (* UNCONDITIONAL JUMP IMM *)
    | {| 0xc3 : 8 |} -> Jump.uncond_imm (next_bytes rom_array pc) state
    (* UNKNOWN INSTRUCTION *)
    | {| _ |} -> unknown_opcode opcode pc
  in
  (* UPDATE TICK COUNT *)
  state.ticks <- (state.ticks + ticks);
  printf "pc[0x%04x]=0x%02x, ticks=%4d, %s\n" pc opcode state.ticks log_msg

let rec run_loop rom_array state =
  let index = UInt16.to_int state.pc in
  decode (rom_array.(index)) rom_array state;
  run_loop rom_array state

let emulate rom_array =
  run_loop rom_array init_game_state
