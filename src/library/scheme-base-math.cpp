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

#include <cmath>
#include "library/scheme-base.h"

extern "C" {

cons_t* proc_abs(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));

  if ( realp(car(p)) ) {
    real_t n = car(p)->real;
    return real(n<0.0? -n : n);
  }

  int n = car(p)->integer;
  return integer(n<0? -n : n);
}

} // extern "C"
