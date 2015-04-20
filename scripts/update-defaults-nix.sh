#!/bin/bash -
#===============================================================================
#
#         USAGE: ./update-defaults-nix.sh
#
#   DESCRIPTION: Updates the default.nix files in all source dirs
#
#  REQUIREMENTS: cabal2nix, bash >= 4
#===============================================================================

set -o nounset
set -o errexit

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
BASE_DIR="$( dirname $DIR)"
SOURCES_TXT="$BASE_DIR/sources.txt"

declare -a SOURCES
readarray -t SOURCES < "$SOURCES_TXT"

for s in ${SOURCES[@]} ; do
    echo $s
    (cd "$BASE_DIR/$s" && cabal2nix . > default.nix )
done
