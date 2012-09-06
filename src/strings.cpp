/*
 * Mickey R7RS Scheme
 *
 * Copyright (C) 2011-2012 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include "strings.h"

std::string string_foldcase(std::string s)
{
  for ( size_t n=0, z=s.length(); n<z; ++n )
    s[n] = tolower(s[n]);

  return s;
}
