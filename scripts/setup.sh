#!/bin/bash

cabal install --only-dependencies &&
cabal configure --enable-tests &&
rm -f .git/hooks/pre-commit &&
chmod +x scripts/pre-commit.sh &&
ln -f scripts/pre-commit.sh .git/hooks/pre-commit
