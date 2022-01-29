(lang dune 2.9)

(name ocaml_typing_benchmark)

(generate_opam_files true)

(license MIT)

(maintainers "ajo41@cam.ac.uk")

(authors "Alistair O'Brien")

(source
 (github johnyob/ocaml-typing-benchmark))

(package
 (name ocaml_typing_benchmark)
 (synopsis "Benchmarking of the OCaml typechecker")
 (depends
  (ocaml
   (= 4.12.0))
  (dune
   (>= 2.9))))
