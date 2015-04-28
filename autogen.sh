#!/bin/bash

echo "Generating autotools files; this may take a while ..."

libtoolize=$(which glibtoolize)
if ! [ -x "$libtoolize" ]; then
  libtoolize=libtoolize
fi

python make-library-m4.py lib > library.m4
$libtoolize --copy
aclocal -I m4 --install
autoheader
automake --include-deps --add-missing --copy
autoconf
echo "You may now run ./configure && make -j"
