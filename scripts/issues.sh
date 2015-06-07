#!/bin/bash

cd meristem

find -type f | while read file; do
  pre="$file:"

  cat "$file" | while read line; do
    if [ -z "$line" ]; then
      exit
    fi
    echo -n "$pre "
    echo "$line"
    pre=$(echo "$pre" | tr [:graph:] " ")
  done
done
