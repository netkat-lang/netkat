(lang dune 1.10)
(using menhir 2.0)
(generate_opam_files true)

(name netkat)
(source (github netkat-lang/netkat))
(license MIT)
(authors
  "Caleb Koch <cak247@cornell.edu>"
  "Steffen Smolka <smolka@cs.cornell.edu>"
  "Nate Foster <jnfoster@cs.cornell.edu>"
)
(maintainers "Steffen Smolka <smolka@cs.cornell.edu>")
(documentation "https://netkat-lang.github.io/netkat/")

(package
  (name netkat)
  (synopsis "A clean slate implementation of NetKAT")
  (description "A clean slate implementation of NetKAT")
  (depends
    (async (>= 0.12.0))
    (ocaml (>= 4.07.0))
    (dune (>= 1.10))
    (menhir (and :build (>= 20190626)))
    (base (>= v0.12.0))
    (core (>= v0.12.0))
    (stdio (>= v0.12.0))
    (odoc :with-doc)
    (open (>= 0))
    (nice_parser (<> 0))
    (idds (<> 0))
    (ppx_deriving (>= 4.3))
    (ppx_inline_test (and :with-test (>= v0.12.0)))
    (ppx_jane (>= v0.12.0))
    (printbox (>= 0.2))
    (tyxml (>= 4.3.0))
    (mparser (>= 1.2.3))
  )
)
