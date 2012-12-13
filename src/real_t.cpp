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

#include <math.h>
#include "types/real_t.h"

real_t minus_infinity()
{
  return log(0);
}

real_t plus_infinity()
{
  return -log(0);
}

bool is_neg_inf(const real_t& n)
{
  return isinf(n) && n<0;
}

bool is_pos_inf(const real_t& n)
{
  return isinf(n) && n>0;
}
