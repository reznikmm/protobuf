#!/bin/bash

set -e -x

PB_DIR=$1
PB_CACHE=${1}_cache

if [ ! -f $PB_DIR/autogen.sh ] ; then
  echo Can not find the protobuf repo
  exit 0
fi

if [ ! -f $PB_CACHE/conformance-test-runner ] ; then
  (cd $PB_DIR;./autogen.sh;./configure)
  make -C $PB_DIR
  make -C $PB_DIR/conformance

  mkdir -p $PB_CACHE
  cp -v --no-dereference \
    $PB_DIR/conformance/.libs/conformance-test-runner \
    $PB_DIR/src/.libs/libprotobuf.so.* \
    $PB_CACHE
fi

for J in conformance/conformance.proto \
 src/google/protobuf/test_messages_proto2.proto \
 src/google/protobuf/test_messages_proto3.proto
do
  PATH=.objs/compiler/:$PATH protoc \
    --ada_out=source/conformance/generated \
    -I`dirname $PB_DIR/$J` $PB_DIR/$J
done

gprbuild -p -P gnat/conformance.gpr

export LD_LIBRARY_PATH=$PB_CACHE

$PB_CACHE/conformance-test-runner \
   --failure_list ada_failing_tests.txt \
  .objs/conformance/conformance-run
