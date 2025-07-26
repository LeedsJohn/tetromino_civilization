(lang dune 3.19)

(name tetromino_civilization)

(generate_opam_files true)

(source
 (github LeedsJohn/tetromino_civilization))

(authors "John Leeds <leedsjohn6@gmail.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license GPL-3.0)

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
   async_js
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
