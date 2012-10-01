#!/bin/bash

echo The following files have lines longer than 120 characters:
find ./ | grep -v ^./dist/ | grep .hs$ | while read line; do
    wc -L $line
done | grep ^[0-9][0-9][0-9][0-9]* | grep -v "^1[01][0-9] " | grep -v "^120 "

if [[ $? != 0 ]]; then
    echo "None!"
else
    exit 1
fi
