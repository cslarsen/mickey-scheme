#!/bin/bash

# Set prefix to your install location
PREFIX=""
CPPFLAGS="-DDEBUG"
CXXFLAGS="-g -O0" # If your compiler supports -Og, use that.
export CPPFLAGS CXXFLAGS

if [ -z "$PREFIX" ]; then
  PREFIX=`pwd`/debug
  if ! [ -d $PREFIX ]; then
    mkdir $PREFIX
  fi
fi

function run() {
  echo $*
  $* || exit 1
}

echo "PREFIX=$PREFIX"
echo "CPPFLAGS=$CPPFLAGS"
echo "CXXFLAGS=$CXXFLAGS"

run ./autogen.sh
run ./configure --prefix=$PREFIX
run make -j
run make -j install

echo "Debug build installed in debug/"
echo "To test, you can run:"
echo "debug/bin/mickey -Itest test/mandelbrot.scm"
