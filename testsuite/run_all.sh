#!/bin/bash
set -e

TESTS=`dirname $0`
OUT="${1:-${TMPDIR:-/tmp}/proto}"
FAILS=0

for J in "$TESTS"/*; do
  if [ -d $J ] ; then
    rm -rf $OUT; mkdir -p $OUT
    PATH="$TESTS/../.objs/compiler/":$PATH protoc --ada_out="$OUT" $J/*.proto
    cp $J/*.ad[sb] "$OUT"
    if ! gprbuild -q -P "$TESTS"/../gnat/test.gpr -XDIR="$OUT" ; then
       echo $J FAILS
       FAILS=1
    else
       echo $J OK
    fi
  fi
done

exit $FAILS
