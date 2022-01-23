(lang dune 2.9)
(name toycheck)
(generate_opam_files true)
(source (github ianhoffman/toycheck))
(license MIT)
(authors "Ian Hoffman")
(maintainers "ianhoffman@gmail.com")

(package
 (name toycheck)
 (synopsis "A toy type checker")
 (depends 
  (dune (>= 2.9.1))
  (ounit2 (>= 2.2.4))))

