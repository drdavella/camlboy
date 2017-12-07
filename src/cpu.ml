open Core.Std

let rec emulate rom_array index rom_size =
  if index < rom_size then
    let byte = rom_array.(index) in
    printf "rom[%04x]=%02x\n" index byte;
    emulate rom_array (index + 1) rom_size
