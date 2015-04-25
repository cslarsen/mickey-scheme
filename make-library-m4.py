#!/usr/bin/env python

import os
import stat

def normalized(s):
    return s.replace("/", "_").replace("-", "_")

def isdir(n):
    return stat.S_ISDIR(os.stat(n).st_mode)

def isfile(n):
    return stat.S_ISREG(os.stat(n).st_mode)

def make(path):
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
    make("lib")
