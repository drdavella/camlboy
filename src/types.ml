open Core
open Unsigned

let rom_start_index = UInt16.of_int 0x100

type real_register =
  | A | B | C | D | E | H | L

type named_register =
  | Reg of real_register
  | HL

let register_array =
  Array.of_list [ Reg B ; Reg C ; Reg D ; Reg E ; Reg H ; Reg L ; HL; Reg A ]

let register_to_str reg =
  match reg with
  | Reg A -> "A"
  | Reg B -> "B"
  | Reg C -> "C"
  | Reg D -> "D"
  | Reg E -> "E"
  | Reg H -> "H"
  | Reg L -> "L"
  | HL -> "HL"

type registers =
  {
    mutable a  : uint16;
    mutable b  : uint16;
    mutable c  : uint16;
    mutable d  : uint16;
    mutable e  : uint16;
    mutable h  : uint16;
    mutable l  : uint16;
  }

type game_state =
  {
    mutable pc : uint16;
    mutable flags : uint8;
    mutable ticks : int;
    memory : UInt16.t Array.t;
    registers : registers;
  }

let init_game_state =
  {
    pc = rom_start_index;
    flags = UInt8.of_int 0;
    ticks = 0;
    memory = Array.create ~len:100 (UInt16.of_int 0);
    registers =
    {
      a = UInt16.of_int 0;
      b = UInt16.of_int 0;
      c = UInt16.of_int 0;
      d = UInt16.of_int 0;
      e = UInt16.of_int 0;
      h = UInt16.of_int 0;
      l = UInt16.of_int 0;
    }
  }
