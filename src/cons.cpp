/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2012 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include <set>

#include "mickey/cons.h"
#include "mickey/exceptions.h"
#include "mickey/garbage-collector.h"

const symbol_t* create_symbol(const std::string& s)
{
  // TODO: Check validity of symbol (e.g., can't begin with number, etc.)
  if ( s.empty() )
    raise(runtime_exception("Symbols must have names"));

  return gc_alloc_symbol(s.c_str());
}
