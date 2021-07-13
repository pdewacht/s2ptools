#!/bin/sh
set -e

VERSION=20210713

CC='wcc -bt=dos -zq -oxhs'
DEFS="-dVERSION=$VERSION"

set -x
python3 genpat.py
ragel -s -G2 scan.rl
$CC $DEFS main.c
$CC $DEFS patch.c
$CC $DEFS scan.c
wlink name s2ppatch system dos file main,patch,scan option quiet
