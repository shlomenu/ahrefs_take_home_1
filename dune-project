(lang dune 3.7)

(name ahrefs_take_home_1)

(generate_opam_files true)

(source
 (github shlomenu/ahrefs_take_home_1))

(authors "Eli Whitehouse")

(license MIT)

(package
 (name ahrefs_take_home_1)
 (depends
  (ocaml
   (= "5.0.0"))
  dune
  unix
  domainslib
  logs
  mtime
  mtime.clock.os))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
