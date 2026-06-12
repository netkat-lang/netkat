# Installation

```
sudo apt install -y autoconf
sudo apt install -y opam
opam init -y
opam switch create 5.3.0
eval $(opam env --switch=5.3.0)

opam install -y dune
opam install -y sedlex landmarks-ppx menhir yojson alcotest core async
opam install -y ego z3

make
```

# Synthesizer Usage

## Generating Synthesis Problem Instances

Use the Enamel encoder to produce an NKPL file from JSON
(see the separate Enamel component for instructions on
installation etc.):

```
hatch run enamel encode large_hospital.json > network-large.nkpl
```

Now do some post-processing to put it in the right format:

```
sed -i "s/^net =.*//g" network-large.nkpl
sed -i "s/^.*(topo.*//g" network-large.nkpl
echo "hole = skip" >> network-large.nkpl
echo "hop = hole ⋅ topo ⋅ pol" >> network-large.nkpl
echo "net = (hop ⋅ δ)⋆" >> network-large.nkpl
```

Finally, create an NKPL file `my-spec.nkpl` that imports your
generated network model:

```
import "network-large.nkpl"
-- add your specification(s) here
check @dev=Provider_Host? ⋅ net ⋅ @dev=Baxter_Sigma_Infusion_Pump ≡ ∅
check @dev=Provider_Host? ⋅ net ⋅ @dev=Medfusion_Infusion_Pump ≢ ∅
```

## Running the Synthesizer

Check out the `z3-synth` branch:
```
git checkout z3-synth
```

Check the properties in `my-spec.nkpl`:

```
dune exec netkat my-spec.nkpl
```

Run the synthesizer on `my-spec.nkpl`:

```
dune exec netkat -- -n 1 -fd -s my-spec.nkpl
```

See other command-line options:

```
dune exec netkat -- --help
```

# Other Use Cases

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
