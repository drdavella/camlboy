open Core

let decode byte =
  let%bitstring bits = {|byte : 8|} in
  match%bitstring bits with
  | {| 0xff : 8 |} -> printf "it's an 0xff!\n"
  | {| _ |} -> printf "unknown code byte=0x%02x\n" byte

let rec emulate rom_array index rom_size =
  if index < rom_size then
    let byte = rom_array.(index) in
    decode byte;
    emulate rom_array (index + 1) rom_size
