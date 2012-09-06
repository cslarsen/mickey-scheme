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

#include "primitives.h"
#include "types/rational_t.h"

rational_t& simplify(rational_t& r)
{
  integer_t d;

  while ( (d = gcd(r.numerator, r.denominator)) != 1 ) {
    r.numerator /= d;
    r.denominator /= d;
  }

  return r;
}

rational_t& rational_t::operator+=(const integer_t& n)
{
  this->numerator += this->denominator*n;
  return simplify(*this);
}

rational_t& rational_t::operator+=(const rational_t& that)
{
  this->numerator = this->numerator*that.denominator +
                    that.numerator*this->denominator;
  this->denominator *= that.denominator;
  return simplify(*this);
}

rational_t operator+(const rational_t& l, const integer_t& r)
{
  rational_t ret;
  ret.numerator = l.numerator + l.denominator*r;
  ret.denominator = l.denominator;
  return ret;
}
