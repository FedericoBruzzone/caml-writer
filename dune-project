(lang dune 3.10)

(name camlwriter)

; (generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name camlwriter)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

(package
  (name core))
(package
  (name windowsize))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
