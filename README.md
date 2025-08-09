# Installation

sudo apt install opam
opam init
opam switch create 5.3.0
eval $(opam env)

opam install -y dune
opam install -y sedlex landmarks-ppx menhir yojson alcotest core

# Usage

NetKAT REPL:

```
dune exec netkat-repl
```

NetKAT command-line:

```
dune exec netkat examples/b.nkpl
```
