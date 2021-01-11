.PHONY: build
build:
	dune build

.PHONY: deps
deps:
	opam install --deps-only -y ./mellified.opam

.PHONY: run
run:
	dune exec src/mellified.exe

