# Setting Up

To build and run Atryka, you will need:

 * Haskell
 * cabal
 * llvm
 * OpenGL
 * SDL
 * SDL\_ttf
 * SDL\_gfx
 * SDL\_mixer

To set up cabal dependencies:

    cabal install --only-dependencies
    cabal configure --enable-tests

And then run `cabal install [package ..]` for each unsatisfied dependency.

# Running

To build the package, simply run

    cabal build

After a successful build, the game can be run from

    dist/build/Atryka/Atryka
