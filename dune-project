(lang dune 2.8)
(generate_opam_files true)

(name garland)
(source (github well-typed-lightbulbs/garland-combinator))
(license MIT)
(authors "Guirlande-OS")
(maintainers "arthur@tarides.com")
(version 0.1)

(package
  (name garland)
  (synopsis "Garland")
  (depends (ocaml (>= "4.08"))))

(package
  (name garland_jsoo)
  (synopsis "Garland, js_of_ocaml playground library")
  (depends
    (ocaml (>= "4.08"))
    js_of_ocaml
    js_of_ocaml-ppx
    js_of_ocaml-toplevel
    js_of_ocaml-compiler))
