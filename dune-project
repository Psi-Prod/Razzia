(lang dune 2.9)

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
 (description
  "Razzia is a Mirage OS friendly library for building Gemini clients")
 (depends
  (dune
   (>= 3.0))
  (ocaml
   (>= 4.14))
  (mirage-clock
   (>= 4.2.0))
  (mirage-random
   (>= 3.0.0))
  (mirage-time
   (>= 3.0.0))
  (tcpip
   (>= 7.1.2))
  (tls
   (>= 0.15.3))
  (tls-mirage
   (>= 0.15.4))
  (uri
   (>= 4.2.0))))

(package
 (name razzia_unix)
 (synopsis "Razzia implementation for Unix and Windows using Lwt")
 (depends
  razzia
  (tcpip
   (>= 7.1.2))))
