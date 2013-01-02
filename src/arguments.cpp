/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2013 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include "arguments.h"
#include "primitives.h"
#include "exceptions.h"
#include "print.h"

size_t min_args(cons_t* p)
{
  size_t n = 0;

  while ( pairp(p) ) {
    p = cdr(p);
    ++n;
  }

  return n;
}

bool variadicp(cons_t* p)
{
  /*
   * We now use proper dot notation so that
   * function signatures are parsed either
   * as a pure list, e.g. (arg1 arg2 arg3)
   * or as a non-proper list that is not
   * terminated with a nil, e.g.
   * (arg1 arg2 . rest-args)
   */
  while ( pairp(p) )
    p = cdr(p);

  return !nullp(p);
}
