(lang dune 2.9)

(version 0.1)

(name razzia)

(license "LGPL-3.0")

(authors "The razzia programmers")

(maintainers "tim.arnouts@protonmail.com" "lelolartichaut@laposte.net")

(source
 (github Psi-Prod/Razzia))

(homepage "https://github.com/Psi-Prod/Razzia")

(generate_opam_files true)

(package
 (name razzia)
 (synopsis "A cross-platform library for building Gemini servers")
 (description "A cross-platform library for building Gemini servers")
 (depends
  (dune
   (>= 3.0))
  (ocaml
   (>= 4.14))))

(package
 (name razzia-mirage)
 (synopsis "Razzia IO implementation for MirageOS")
 (depends
  razzia
  (dns-client
   (>= 6.4.1))
  (mirage-channel
   (>= 4.1.0))
  (mirage-random
   (>= 3.0.0))
  (mirage-time
   (>= 3.0.0))
  (tls
   (>= 0.15.4))
  (tls-mirage
   (>= 0.15.4))))

(package
 (name razzia-unix)
 (synopsis "Razzia IO implementation using UNIX bindings")
 (depends
  razzia
  razzia-mirage
  (mirage-clock-unix
   (>= 4.2.0))
  (mirage-random
   (>= 3.0.0))
  (mirage-random-stdlib
   (>= 0.1.0))
  (mirage-time-unix
   (>= 3.0.0))))

(package
 (name razzia-eio)
 (synopsis "Razzia IO implementation using Eio")
 (depends
  razzia
  (eio
   (>= 0.6))
  (tls-eio
   (>= 0.15.5))))
