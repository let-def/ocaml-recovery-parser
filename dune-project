(lang dune 1.11)
(using menhir 2.0)
(generate_opam_files true)

(package
 (name recovery_parser)
 (depends
  fix
  menhir))
