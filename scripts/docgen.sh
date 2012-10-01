#!/bin/bash

echo "Files with incomplete Haddock coverage:"
cabal haddock --executables | grep "^  [ 0-9][0-9]%"

ret=$PIPESTATUS

if [[ $? == 0 ]]; then
    exit $ret
else
    echo "None!"
fi
