from ubuntu:18.04

RUN apt-get update
RUN apt install -y software-properties-common
RUN apt-get update
RUN add-apt-repository --yes ppa:avsm/ppa
RUN apt install -y \
  opam \
  gcc \
  make \
  vim \
  less

RUN adduser --shell /bin/bash devuser
USER devuser
WORKDIR /home/devuser/toycheck

# No need for sandboxing because Docker is already a sandbox
RUN opam init --disable-sandboxing
# Useful deps
RUN opam install -y \
  dune \
  ocamlformat
# Required for LSP integration with VSCode
RUN opam install -y \
  merlin \
	ocaml-lsp-server \
	ocamlformat-rpc