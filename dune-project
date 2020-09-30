(lang dune 2.7.1)
(name progress)
(implicit_transitive_deps false)

(generate_opam_files true)
(source (github CraigFe/progress))
(maintainers "Craig Ferguson <me@craigfe.io>")
(authors "Craig Ferguson <me@craigfe.io>")

(package
 (name progress)
 (synopsis "Dynamic progress bars")
 (description "\
Dynamic progress bars
")
 (documentation https://CraigFe.github.io/progress/)
 (depends alcotest fmt logs))
