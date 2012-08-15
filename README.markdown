# Setting Up

To build and run the game, you will need:

 * Haskell
 * cabal
 * llvm
 * OpenGL
 * GLFW

To set up cabal dependencies:

    cabal install --only-dependencies

# Building

First, configure the project with

    cabal configure

If there are unsatisfied dependencies, run `cabal install [dependency ..]` for each one.
When the project has been successfully configured, it can be built with

    cabal build

# Running

To build the package, run

    cabal build

After a successful build, the game can be run from

    dist/build/Game2D/Game2D

# Tests

To compile with tests, simply add the `--enable-test` flag when configuring.
To run the tests after a successful build, run

    cabal test

# Code Standards #

The `Util` and `Wrappers` folders are for code which is *not project-specific*:
Direct library wraps go into `Wrappers/`, and useful generic functions and modules go in `Util/`.
