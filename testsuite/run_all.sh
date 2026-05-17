#!/bin/bash
set -e

TESTS=`dirname $0`
RUNTIME="${2:-league}"
OUT="${1:-${TMPDIR:-/tmp}/proto_$RUNTIME}"
FAILS=0

for J in "$TESTS"/*; do
  if [ -d $J ] ; then
    rm -rf $OUT; mkdir -p $OUT
    PATH="$TESTS/../.objs/compiler/":$PATH \
        protoc -I$J --ada_out="$OUT" --ada_opt=runtime=$RUNTIME $J/*.proto
    cp $J/*.ad[sb] "$OUT"
    if ! gprbuild -q -P "$TESTS"/../gnat/test_$RUNTIME.gpr -XDIR="$OUT" ; then
       echo $J FAILS $RUNTIME
       FAILS=1
    else
       echo $J OK $RUNTIME
    fi
  fi
done

exit $FAILS
