#!/bin/sh -e
in=${1?No input file specified}
mv $in ${bak=.$in.bak}
shift
"$@" < $bak > $in
