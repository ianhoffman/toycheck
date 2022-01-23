(lang dune 2.9)
(generate_opam_files true)

(name toycheck)
(source (github ianhoffman/toycheck))
(license MIT)
(authors "Ian Hoffman")
(maintainers "ianhoffman@gmail.com")

(package
 (name toycheck)
 (synopsis "A toy type checker")
 (description "A toy type checker")
 (depends 
  (dune (>= 2.9.1))
  (ounit2 :with-test)))
