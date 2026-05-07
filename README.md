# Installation

```
sudo apt install -y autoconf
sudo apt install -y opam
opam init -y
opam switch create 5.3.0
eval $(opam env --switch=5.3.0)

opam install -y dune
opam install -y sedlex landmarks-ppx menhir yojson alcotest core async
opam install -y ego

make
```

# Usage

NetKAT REPL:

```
dune exec netkat-repl
```

NetKAT command-line:

```
dune exec netkat examples/b.nkpl
```

NetKAT TCP server:

```
dune exec netkat-listen
nc localhost 8080
```

NetKAT Egg test:

```
dune exec netkat-egg
```

Parse all examples:
```
make parse
tail -f parsed.txt
```
