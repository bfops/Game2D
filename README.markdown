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

    scripts/build.sh

# Running

After a successful build, the game can be run from

    dist/build/Game2D/Game2D

# Documentation

Haddock documentation can be generated using

    scripts/docgen.sh

By default, the documentation is generated to `dist/docs/html/`

# Tests

To compile with tests, simply add the `--enable-test` flag when configuring.
To run the tests after a successful build, run

    scripts/test.sh

# Code Standards #

The `Util` and `Wrappers` folders are for code which is *not project-specific*:
Direct library wraps go into `Wrappers/`, and useful generic functions and modules go in `Util/`.

Functions ending with a single quote usually require a transformation function as one of their parameters.
