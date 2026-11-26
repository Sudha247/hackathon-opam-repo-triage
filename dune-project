(lang dune 3.0)

(name opam-ci-triage)

(generate_opam_files true)

(package
 (name opam-ci-triage)
 (synopsis "Triage opam-repo-ci failures of ocaml/opam-repository pull requests")
 (depends
  (ocaml (>= 5.1))
  dune
  (re (>= 1.10.0))
  (lambdasoup (>= 1.0.0))
  (uucp (>= 15.0.0))
  (uri (>= 4.0.0))
  (bos (>= 0.2.0))
  (fpath (>= 0.7.0))
  (ezcurl (>= 0.3))
  (yojson (>= 2.0.0))
  camlzip
  digestif
  ptime
  (cmdliner (>= 1.1.0))
  (alcotest :with-test)))
