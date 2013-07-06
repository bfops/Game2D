# Setting Up

To build and run the game, you will need:

 * Haskell
 * cabal
 * llvm
 * OpenGL
 * GLFW
 * Summit (https://github.com/RobotGymnast/Summit)

You can set up the build environment by running

    scripts/setup.sh

Doing so is **mandatory** before committing any code, as this also sets up pre-commit hooks.
All scripts should be run from the project root directory.

# Building

When the environment has been successfully set up, the project can be built with

    scripts/build.sh

# Running

After a successful build, the game can be run from

    dist/build/Game2D/Game2D

# Documentation

Haddock documentation can be generated using

    scripts/docgen.sh

By default, the documentation is generated to `dist/docs/html/`

# Tests

To run the tests after a successful build, run

    scripts/test.sh

# Code Standards

The `Util` and `Wrappers` folders are for code which is *not project-specific*:
Direct library wraps go into `Wrappers/`, and useful generic functions and modules go in `Util/`.

Coding is a language. You are expressing ideas, so they should be as clear, concise, and elegant as possible.

 * Wrap to 120 characters
 * Functions ending with a single quote usually require a transformation function as one of their parameters
 * When indenting multi-lined bodies, align SOMETHING visually (e.g. operators)
   or just use a multiple of four spaces (at least 8)
 * Indent a `where` clause by 4 spaces
 * If a `where` clause has more than one line in it, the `where` keyword should be on a distinct line from any code
 * Do not have more than one embedded subscope (A `let` inside a `where` is acceptable, but to be used sparingly)
