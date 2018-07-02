#!/bin/sh
set -e

eval $(grep ^VERSION= build.sh)
DEFS="-DVERSION=$VERSION"

python3 genpat.py > patterns.rl
ragel -s -G2 patch.rl
cc -Wall -O2 $DEFS -o s2ppatch main.c patch.c
