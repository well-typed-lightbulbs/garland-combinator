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
