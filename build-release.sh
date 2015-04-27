#!/bin/bash

# Set prefix to your install location
PREFIX=""
CPPFLAGS="-DNDEBUG"
CXXFLAGS="-O3"
export CPPFLAGS CXXFLAGS

if [ -z "$PREFIX" ]; then
  PREFIX=`pwd`/release
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
