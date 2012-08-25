#!/bin/bash

cabal test | grep "^Test suite" | grep -v ^"Test suite logged" | grep -v ": RUNNING.."
