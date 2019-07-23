# NetKAT
A clean slate implementation of [NetKAT](http://www.cs.cornell.edu/~jnfoster/papers/frenetic-netkat.pdf).

## Documentation
You can generate documentation from the source code. You need to have OCaml's packet manager `opam` and the packages `dune` and `odoc`installed:
```
opam install dune odoc
```
Then, from the root of the project:
```
dune build @doc
```
Then open `_build/default/_doc/_html/index.html`.

## References
* [NetKAT: Semantic Foundations for Networks](http://www.cs.cornell.edu/~jnfoster/papers/frenetic-netkat.pdf). Anderson, C. J., Foster, N., Guha, A., Jeannin, J. B., Kozen, D., Schlesinger, C., & Walker, D. POPL 2014.
* [A Coalgebraic Decision Procedure for NetKAT](http://www.cs.cornell.edu/~jnfoster/papers/netkat-automata.pdf). Foster, N., Kozen, D., Milano, M., Silva, A. and Thompson, L. POPL 2015.
* [A Fast Compiler for NetKAT](https://www.cs.cornell.edu/~jnfoster/papers/netkat-compiler.pdf). Smolka, S., Eliopoulos, S., Foster, N. and Guha, A. ICFP 2015.
