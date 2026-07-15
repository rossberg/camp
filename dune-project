(lang dune 3.23)

(authors "Andreas Rossberg")
(maintainers "rossberg@mpi-sws.org")
(license "CC-BY-NC-SA-4.0")
(source (github rossberg/camp))
(homepage https://mpi-sws.org/~rossberg/camp/)
(documentation https://github.com/rossberg/camp/blob/main/README.txt)
(maintenance_intent "(latest)")

(generate_opam_files)

(package
  (name camp)
  (version 0.9.6)
  (synopsis "Camp music player")
  (description "A portable music player in the Winamp tradition")
  (depends
    (ocaml (>= 5.4.0))
    (raylib (>= 2.2.1))
    (raylib-callbacks (>= 2.2.1))
    (uunf (>= 17.0.0))
    (uucp (>= 17.0.0))
    (confero (>= 0.1.1))
    (directories (>= 0.6))
  )
)
