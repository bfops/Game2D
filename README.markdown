# Setting Up

To build and run the game, you will need:

 * Haskell
 * cabal
 * llvm
 * OpenGL
 * GLFW

To set up cabal dependencies:

    cabal install --only-dependencies
    cabal configure --enable-tests

And then run `cabal install [package ..]` for each unsatisfied dependency.

# Does it work?

To build the package, simply run

    cabal build

Run the test suite with

    cabal test

After a successful build, the game can be run from

    dist/build/Game2D/Game2D

# Code Standards #

The `Util` and `Wrappers` folders are for code which is *not project-specific*:
Direct library wraps go into `Wrappers/`, and useful generic functions and modules go in `Util/`.
