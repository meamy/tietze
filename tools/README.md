# Lafont

A tool for string rewriting.
Documentation is a work in progres.
See the examples directory for more information.

## Building

This project is built using `cabal`.
The simplest way to get started with `cabal` is to run the following.
1. `apt-get install cabal-install`
2. `cabal update`
3. `cabal install cabal`
After setting up `cabal`, the project can be built with `cabal install`.
Note that some dependencies, such as `yaml`, are resource extensive to build.
As a resutl, `cabal install` might fail due to exceeding memory limits.
In this case, the `--ghc-options="+RTS -M1200M` can be used to increase the memory limit.
