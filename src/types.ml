open Unsigned

let rom_start_index = UInt16.of_int 0x100

type game_state =
  {
    mutable pc : uint16;
    mutable flags : uint8;
    mutable ticks : int;
  }

let init_game_state =
  {
    pc = rom_start_index;
    flags = UInt8.of_int 0;
    ticks = 0;
  }
