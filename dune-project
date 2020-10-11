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
 (documentation https://CraigFe.github.io/progress/)
 (depends
  (ocaml (>= "4.08.0"))
  mtime
  terminal_size
  (alcotest (and (>= "1.1.0") :with-test))
  (astring :with-test)
  (fmt :with-test)))
