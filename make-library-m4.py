#!/usr/bin/env python

"""
Recursively adds all files in a library to automake.

Usage example::

    $ python make-library-m4.py lib > library.m4

Then, in Makefile.am, add::

    include library.m4

This file is part of Mickey Scheme
Copyright (C) 2015 Christian Stigen Larsen
Distributed under the LGPL v2.1 or later.
"""

import os
import stat
import sys

def normalized(s):
    """Normalizes string for inclusion in an automake file."""
    return s.replace("/", "_").replace("-", "_")

def isdir(path):
    """Checks if given path is a directory."""
    return stat.S_ISDIR(os.stat(path).st_mode)

def isfile(path):
    """Checks if given path is a file."""
    return stat.S_ISREG(os.stat(path).st_mode)

def make(path):
    """Recursively prints out automake build commands for a directory tree."""
    files = []
    subdirs = []

    for f in os.listdir(path):
        name = os.path.join(path, f)
        if isfile(name):
            files.append(name)
        elif isdir(name):
            subdirs.append(name)

    if len(files):
        print("lib_%sdir = $(pkgdatadir)/$(VERSION)/%s" % (normalized(path),
            path))
        print("lib_%s_DATA = \\" % normalized(path))
        for i, f in enumerate(sorted(files)):
            print("\t%s%s" % (f, " \\" if (i+1) < len(files) else ""))
        print("")

    if len(subdirs):
        for s in sorted(subdirs):
            make(s)

if __name__ == "__main__":
    for directory in sys.argv[1:]:
        make(directory)
