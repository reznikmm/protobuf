#!/bin/bash

set -e -x

PB_DIR=$1
PB_CACHE=${1}_cache

if [ ! -f $PB_DIR/conformance/conformance.proto ] ; then
  echo Can not find the protobuf repo
  exit 0
fi

if [ ! -f $PB_CACHE/conformance-test-runner ] ; then
  (cd $PB_DIR; cmake . -Dprotobuf_BUILD_CONFORMANCE=ON && cmake --build .)

  mkdir -p $PB_CACHE
  cp -v --no-dereference \
    $PB_DIR/_deps/jsoncpp-build/src/lib_json/libjsoncpp.so.26 \
    $PB_DIR/conformance_test_runner \
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

$PB_CACHE/conformance_test_runner \
   --failure_list ada_failing_tests.txt \
  .objs/conformance/development/conformance-run
