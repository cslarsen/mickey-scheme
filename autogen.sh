#!/bin/bash

echo "Generating autotools files"

python make-library-m4.py lib > library.m4
autoreconf -vi
