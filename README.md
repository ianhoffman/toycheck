# Toycheck

An OCaml clone of https://github.com/igstan/itake-2015.

## Getting Started

* Run `docker-compose build` and wait awhile.
* Once that's done, get a prompt in the container with `docker-compose run toycheck /bin/bash`.
* Initialize opam: `eval $(opam env)`.
* Install project dependencies: `opam install -y . --deps-only --with-test`.
* Build the project: `dune build`.
* Open up VSCode and, if you haven't already, install the [VSCode OCaml Platform](https://github.com/ocamllabs/vscode-ocaml-platform) extension.
* Connect to the `toycheck` container in the "Remote Explorer" tab in VSCode. You'll want to open a project in the `home/devuser/toycheck` directory. 

That's it! You should be set up with autocompletion and ready to go.

Obviously I don't expect anyone but myself to work on this project, since it's just a way for me to learn OCaml. But I do think that this is a relatively nice and easy OCaml setup which can be reused for other projects.

## TODO

* Parser, inference, all that good stuff.
* Setup "OCaml Platform" VSCode extension when building docker container (maybe using `devcontainer.json`).
* Experiment with using Cram tests instead of OUnit.
