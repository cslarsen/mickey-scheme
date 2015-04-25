#!/bin/bash

echo "Generating autotools files; this may take a while ..."
glibtoolize
aclocal
autoheader
automake --include-deps --add-missing --copy
autoconf
echo "You may now run ./configure && make -j"
