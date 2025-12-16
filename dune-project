(lang dune 3.20)

(name camp)
(authors "Andreas Rossberg")
(source (github rossberg/camp))
(license CC-BY-NC-SA-4.0)
(documentation https://github.com/rossberg/camp/blob/main/README.txt)

(package
  (name camp)
  (synopsis "Camp")
  (description "A music player")
  (depends
    (ocaml (>= 5.04)) dune
    (raylib (>= 1.6.0)) (raylib-callbacks (>= 1.6.0))
    uunf uucp confero directories
  )
)
