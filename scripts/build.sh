#!/bin/bash

cabal build | grep -v "^Loading package"
exit $PIPESTATUS
