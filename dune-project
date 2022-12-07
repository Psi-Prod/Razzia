(lang dune 3.0)

(version 0.1)

(name razzia)

(license "GPL-3.0")

(authors "The razzia programmers")

(maintainers "tim.arnouts@protonmail.com" "lelolartichaut@laposte.net")

(source
 (github Psi-Prod/Razzia))

(homepage "https://github.com/Psi-Prod/Razzia")

(generate_opam_files true)

(package
 (name razzia)
 (synopsis "A Mirage OS friendly library for building Gemini clients")
 (description "")
 (depends
  (dune
   (>= 3.0))
  (ocaml
   (>= 4.14))))
