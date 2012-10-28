#!/bin/bash

echo "Files with incomplete Haddock coverage:"
cabal haddock --executables | grep "^  [ 0-9][0-9]%"

ret=$?
pret=$PIPESTATUS

# error running cabal
if [[ $pret != 0 ]]; then
    exit $pret
fi

# grep found something; there are incomplete files
if [[ $ret == 0 ]]; then
    exit 1
fi

echo "None!"
