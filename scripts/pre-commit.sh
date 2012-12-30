#!/bin/bash

dir=/tmp/testbuild/`pwd | sed "s/.*\///g"`
if [ -d "$dir" ]; then
    rm -rf "$dir"/.git
    echo Re-using build environment in "$dir"
else
    rm -rf "$dir" &&
    echo Creating temporary build environment in "$dir"
fi &&

mkdir -p "$dir"
cp -r --preserve=timestamps . "$dir" &&
cd "$dir" &&
rm -f .git/hooks/pre-commit &&
git commit -m "Temp" > /dev/null &&
git reset --hard HEAD > /dev/null &&
git clean -fd > /dev/null &&
scripts/setup.sh &&
scripts/build.sh &&
echo &&
scripts/test.sh &&
echo &&
scripts/linecheck.sh &&
echo &&
rm -rf "$dir"
