/*
 * Mickey Scheme
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *                                                          
 */

#include <sys/stat.h>
#include <string>
#include "file-io.h"

std::string slurp(FILE *f)
{
  std::string r;

  for ( int c; (c = fgetc(f)) != EOF; )
    r += c;

  return r;
}

bool file_exists(const std::string& s)
{
  struct stat st;
  return !stat(s.c_str(), &st);
}
