#!/bin/bash

# This is what I use to build Mickey. It will choose default settings for
# install location and compilation flags.
#
# You can override these by setting them before invoking this script.
# For example: PREFIX=foo CXXFLAGS="-g -O2" build-release.sh

# Set prefix to your install location.
DEFAULT_PREFIX=`pwd`/build-release
PREFIX=${PREFIX:-$DEFAULT_PREFIX}

# Set compilation flags
CPPFLAGS=${CPPFLAGS:-"-DNDEBUG"}
CXXFLAGS=${CXXFLAGS:-"-O3 -march=native -mtune=native -ffast-math"}

# Make these available to configure
export CPPFLAGS CXXFLAGS

function run() {
  echo $*
  $* || exit 1
}

echo "PREFIX=$PREFIX"
echo "CPPFLAGS=$CPPFLAGS"
echo "CXXFLAGS=$CXXFLAGS"

if [ -z "$PREFIX" ]; then
  echo "Creating $PREFIX"
  mkdir -p $PREFIX
fi

run ./autogen.sh
run ./configure --prefix=$PREFIX
run make -j || exit 1
run make -j check || exit 1

if [ ${PREFIX} == ${DEFAULT_PREFIX} ]; then
  run make -j install
  echo "Installed in $PREFIX"
else
  echo "Run make -j install to install to $PREFIX"
fi
