# camlboy
WIP GameBoy emulator implemented in OCaml. This probably won't go much further,
since I'm not sure that I can optimize this to be anywhere near performant
enough to actually play a game. That's a shame, because I've really grown to
like OCaml as a language.

# Setup

This project currently is using OCaml version 4.04.0 since `ppx_bitstring`
wouldn't install properly on 4.06.0.

Assuming that you have OCaml and `opam` installed:
```
$ opam switch 4.04.0
$ eval `opam config env`
$ opam install core
$ opam install ppx_bitstring
$ opam install ocamlfind
```
