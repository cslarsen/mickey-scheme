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

#include "mickey/circular.h"
#include "mickey/primitives.h"
#include "mickey/print.h"

/*
 * Detect cycles using Floyd's algorithm:
 * http://en.wikipedia.org/wiki/Cycle_detection
 */
bool circularp(const cons_t* t)
{
  const cons_t *h = t;

  do {
    t = cdr(t);  // slow tortoise
    h = cddr(h); // fast hare
  } while ( t != h );

  return !nullp(t);
}
