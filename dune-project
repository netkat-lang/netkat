(lang dune 3.19)
(name netkat)
(using menhir 2.0)
(documentation mld_files)

(version 0.1.4)

(generate_opam_files true)

(source
 (github netkat-lang/netkat))

(authors
 "Mark Moeller <moeller@cs.cornell.edu>"
 "Nate Foster <jnfoster@cs.cornell.edu>"
 "Alexandra Silva <alexandra.silva@cornell.edu>"
 "Jed McClurg <jrmcclurg@cornell.edu>"
 "Alaia Solko-Breslin <alaia@seas.upenn.edu>"
 "Caleb Koch <ckoch@stanford.edu>")

(maintainers
 "jrmcclurg@cornell.edu")

(license Apache-2.0)

(homepage "https://github.com/netkat-lang/netkat")

(bug_reports "https://github.com/netkat-lang/netkat")

(package
 (name netkat)
 (synopsis
  "NetKAT: A library for manipulating Finite Automata and Regular Expressions")
 (depends
 (ocaml (>= "5.3.0"))
 (alcotest (>= "1.9.0"))
 (dune (>= "3.19.1"))
 (core (>= "v0.17.0"))
 (async (>= "v0.17.0"))
 (yojson (>= "3.0.0"))
 (sedlex (>= "3.6"))
 (menhir (and :build (>= "20240715")))
 (landmarks-ppx (>= "1.5"))
 (landmarks (>= "1.5"))
 (ego (>= "0.0.6"))))
