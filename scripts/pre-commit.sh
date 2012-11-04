#!/bin/bash

rm -rf /tmp/testbuild &&
mkdir /tmp/testbuild &&
cp -r `ls -A --color=never | grep -v "^dist$"` /tmp/testbuild/ &&
cd dist &&
mkdir /tmp/testbuild/dist
cp -r `ls -A --color=never | grep -v "^build$"` /tmp/testbuild/dist &&
cd /tmp/testbuild &&
rm -f .git/hooks/pre-commit &&
git commit -m "Temp" > /dev/null &&
git reset --hard HEAD > /dev/null &&
git clean -fd &&
scripts/setup.sh &&
scripts/build.sh &&
echo &&
scripts/test.sh &&
echo &&
scripts/linecheck.sh
