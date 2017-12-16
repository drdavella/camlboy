open Core
open Types

let load_register dest source code_bytes state =
      let msg = sprintf "HEY GUYS! 0x%1x 0x%1x\n" dest source in
      (Utils.increment_pc state 1), 4, msg
