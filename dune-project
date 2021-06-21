(lang dune 2.7.1)
(name progress)
(implicit_transitive_deps false)

(generate_opam_files true)
(source (github CraigFe/progress))
(maintainers "Craig Ferguson <me@craigfe.io>")
(authors "Craig Ferguson <me@craigfe.io>")

(package
 (name progress)
 (synopsis "User-definable progress bars")
 (description "\
A progress bar library for OCaml, featuring a DSL for declaratively specifying
progress bar formats. Supports rendering multiple progress bars simultaneously.\
")
 (documentation https://CraigFe.github.io/progress/)
 (depends
  (ocaml (>= 4.08.0))
  mtime
  terminal_size
  uutf
  uucp
  (optint (>= 0.1.0))
  (alcotest (and (>= 1.1.0) :with-test))
  (astring :with-test)
  fmt
  vector)
 (depopts logs))
