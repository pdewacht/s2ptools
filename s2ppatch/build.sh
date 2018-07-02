#!/bin/sh
set -e

VERSION=20180503

CC='wcc -bt=dos -zq -oxhs'
DEFS="-dVERSION=$VERSION"

set -x
python3 genpat.py > patterns.rl
ragel -s -G2 patch.rl
$CC $DEFS main.c
$CC $DEFS patch.c
wlink name s2ppatch system dos file main,patch option quiet
