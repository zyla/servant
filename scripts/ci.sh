#!/usr/bin/env bash

set -o nounset
set -o errexit
set -o verbose

stack test --ghc-options=-Werror --no-terminal --coverage

# send test coverage statistics to coveralls
if ! which shc ; then
  tmpDir=$(mktemp -d)
  (cd $tmpDir &&
   wget https://github.com/rubik/stack-hpc-coveralls/releases/download/v0.0.4.0/shc-linux-x64-8.0.1.tar.bz2 &&
   tar -xf shc-linux-x64-8.0.1.tar.bz2 &&
   cp shc ~/.local/bin)
fi
which shc

shc combined all
