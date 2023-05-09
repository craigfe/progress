(lang dune 2.7)
(name progress)
(implicit_transitive_deps false)

(generate_opam_files true)
(source (github CraigFe/progress))
(license MIT)
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
  (terminal (= :version))
  (fmt (>= 0.8.5))
  (logs (>= 0.7.0))
  (mtime (>= 2.0.0))
  (uucp (>= 2.0.0))
  (uutf (>= 1.0.0))
  vector
  (optint (>= 0.1.0))
  (alcotest (and :with-test (>= 1.4.0)))
  (astring :with-test)))

(package
 (name terminal)
 (synopsis "Basic utilities for interacting with terminals")
 (description "Basic utilities for interacting with terminals")
 (documentation https://CraigFe.github.io/progress/)
 (depends
  (ocaml (>= 4.03.0))
  (uucp (>= 2.0.0))
  (uutf (>= 1.0.0))
  stdlib-shims
  (alcotest (and :with-test (>= 1.4.0)))
  (fmt :with-test)
  (astring :with-test)
  (mtime (and :with-test (>= 2.0.0)))))
