#!/usr/bin/env bash

set -o nounset
set -o errexit
set -o verbose

stack test --ghc-options=-Werror --no-terminal --coverage

# send test coverage statistics to coveralls
stack install stack-hpc-coveralls-0.0.4.0 --no-terminal
shc combined all
