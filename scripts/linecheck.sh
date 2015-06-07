#!/bin/bash

LINES=120

echo The following files have lines longer than $LINES characters:
find ./ | grep -v ^./dist/ | grep .hs$ | while read line; do
    wc -L $line
done | grep ^[0-9][0-9][0-9][0-9]* | grep -v "^1[01][0-9] " | grep -v "^$LINES "

if [[ $? != 0 ]]; then
    echo "None!"
else
    exit 1
fi
