# Theory of Programming Languages (NPRG085)

This repository contains implementations of various concepts discussed during the lectures. The package doesn't contain an executable; it is primarily meant to be used in a REPL environment.

To start out, run:

    cabal repl

This should start GHCI and load the module `Untyped`. If you want to mess with the type inference, load the `Typed` module by running the following in your GHCI:

    :m *Typed

The package is also fully documented. You can generate the documentation by running:

    cabal haddock
