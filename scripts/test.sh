#!/bin/bash

cabal test | grep -v ^"Test suite logged" | grep -v ": RUNNING.." | grep -v "^Running " | grep -v "^[0-9]* of [0-9]* test suites"
exit $PIPESTATUS
