#!/bin/bash

echo "Haddock coverage:"
cabal haddock --executables 2>&1 | grep "^ [ 0-9][ 0-9][0-9]%"
exit $PIPESTATUS
