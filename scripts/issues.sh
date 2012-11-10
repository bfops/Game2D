#!/bin/bash

cd meristem

if [ -d bugs ]; then
    for bug in bugs/*; do
        echo "$bug": `head -1 "$bug"`
    done
fi

if [ -d improvements ]; then
    for i in improvements/*; do
        echo "$i": `head -1 "$i"`
    done
fi
