#!/bin/bash

echo "Haddock coverage:"
cabal haddock --executables | grep "^ [ 0-9][ 0-9][0-9]%"
exit $PIPESTATUS
