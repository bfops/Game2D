#!/bin/bash

cabal build $@ 2>&1 | grep -v "^\(Loading package\|You are using a new version of LLVM that hasn't been tested yet!$\|We will try though...$\)"
exit $PIPESTATUS
