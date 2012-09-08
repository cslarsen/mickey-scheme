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

#ifndef INC_MICKEY_RATIONAL_H
#define INC_MICKEY_RATIONAL_H

#include "integer_t.h"

struct rational_t {
  integer_t numerator;
  integer_t denominator;

  friend rational_t operator+(const rational_t& l, const integer_t& r);
  rational_t& operator+=(const integer_t& n);
  rational_t& operator+=(const rational_t& that);
  rational_t& operator*=(const integer_t& n);
  rational_t& operator*=(const rational_t& that);
};

rational_t& simplify(rational_t& r);

#endif
