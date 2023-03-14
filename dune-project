(lang dune 3.0)

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
 (synopsis "A cross-platform library for building Gemini client")
 (description "A cross-platform library for building Gemini client")
 (depends
  (ocaml
   (>= 4.14))
  (mirage-clock
   (>= 4.2.0))
  (uri
   (>= 4.2.0))
  (re
   (>= 1.10.4))
  (tls (>= 0.16.0))
  (x509 (>= 0.16.4))))

(package
 (name razzia-mirage)
 (synopsis "Razzia IO implementation for MirageOS")
 (depends
  razzia
  (dns-client-mirage
   (>= 7.0.0))
  (mirage-channel
   (>= 4.1.0))
  (mirage-random
   (>= 3.0.0))
  (mirage-time
   (>= 3.0.0))
  (tls-lwt
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
   (>= 3.0.0))
  (tcpip
   (>= 7.1.2))))

(package
 (name razzia-eio)
 (synopsis "Razzia IO implementation using Eio")
 (depends
  razzia
  (eio
   (>= 0.6))
  (mirage-clock-unix
   (>= 4.2.0))
  (tls-eio
   (>= 0.15.5))))
