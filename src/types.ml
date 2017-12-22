open Core
open Unsigned

exception NotImplemented of string

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
    mutable a  : uint8;
    mutable b  : uint8;
    mutable c  : uint8;
    mutable d  : uint8;
    mutable e  : uint8;
    mutable h  : uint8;
    mutable l  : uint8;
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
      a = UInt8.of_int 0;
      b = UInt8.of_int 0;
      c = UInt8.of_int 0;
      d = UInt8.of_int 0;
      e = UInt8.of_int 0;
      h = UInt8.of_int 0;
      l = UInt8.of_int 0;
    }
  }
