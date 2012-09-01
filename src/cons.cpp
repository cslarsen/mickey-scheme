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
#include "cons.h"
#include "exceptions.h"

typedef std::set<symbol_t> symbols_t;
static symbols_t symbols;

const symbol_t* create_symbol(const std::string& s)
{
  // TODO: Check validity of symbol (e.g., can't begin with number, etc.)
  if ( s.empty() )
    raise(runtime_exception("Symbols must have names"));

  symbols_t::iterator i = symbols.find(s);

  // return symbol if it already exists
  if ( i != symbols.end() )
    return &(*i); // return it

  // if not, create it
  std::pair<symbols_t::iterator, bool> p = symbols.insert(s);
  return &(*p.first);
}
