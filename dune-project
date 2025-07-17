(lang dune 3.19)

(name tetromino_civilization)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name tetromino_civilization)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   (merlin :dev)
   (ocaml-lsp-server :dev)
   (ocamlformat :dev)
   (utop :dev)
   async
   bonsai
   core
   core_unix
   js_of_ocaml
   js_of_ocaml-ppx
   ocaml
   ppx_css)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
