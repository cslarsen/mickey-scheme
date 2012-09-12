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
#include "exceptions.h"

rational_t& simplify(rational_t& r)
{
  if ( r.denominator == 1 )
    return r;

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

rational_t& rational_t::operator+=(const rational_t& n)
{
  this->numerator = this->numerator*n.denominator +
                    n.numerator*this->denominator;
  this->denominator *= n.denominator;
  return simplify(*this);
}

rational_t& rational_t::operator*=(const integer_t& n)
{
  this->numerator *= n;
  return simplify(*this);
}

rational_t& rational_t::operator*=(const rational_t& n)
{
  /*
   * TODO: This could cause overflowing because we do simplification in the
   * last step.  A way to fix this is to detect common factors beforehand,
   * and only multiply those n are not.
   */
  this->numerator *= n.numerator;
  this->denominator *= n.denominator;
  return simplify(*this);
}

rational_t& rational_t::operator/=(const rational_t& d)
{
  numerator *= d.denominator;
  denominator *= d.numerator;
  return simplify(*this);
}

rational_t operator+(const rational_t& l, const integer_t& r)
{
  rational_t ret;
  ret.numerator = l.numerator + l.denominator*r;
  ret.denominator = l.denominator;
  return ret;
}

bool rational_t::operator==(const rational_t& r) const
{
  return numerator == r.numerator && denominator == r.denominator;
}

rational_t operator/(const rational_t& n, const rational_t& d)
{
  if ( d.numerator == 0 )
    raise(runtime_exception("Division by zero"));

  rational_t q(n);
  q /= d;

  return q;
}
