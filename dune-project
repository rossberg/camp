(lang dune 3.5)

(name kamp)
(authors "Andreas Rossberg")
(source (github rossberg/kamp))
(license CC-BY-NC-SA-4.0)
(documentation https://github.com/rossberg/kamp/blob/main/README.txt)

(package
  (name kamp)
  (synopsis "Kamp")
  (description "A music player")
  (depends (ocaml (>= 5.04)) dune (raylib (>= 1.5.1)) uunf uucp confero directories)
)
