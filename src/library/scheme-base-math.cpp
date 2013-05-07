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

#include <stdlib.h>
#include <cmath>
#include "library/scheme-base.h"

extern "C" {

cons_t* proc_abs(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));

  if ( realp(car(p)) ) {
    real_t n = car(p)->number.real;
    return real(n<0.0? -n : n);
  }

  int n = car(p)->number.integer;
  return integer(n<0? -n : n);
}

cons_t* proc_addf(cons_t *p, environment_t*)
{
  real_t sum = 0.0;

  for ( ; !nullp(p); p = cdr(p) ) {
    cons_t *i = listp(p)? car(p) : p;

    if ( integerp(i) )
      sum += static_cast<real_t>(i->number.integer);
    else if ( realp(i) )
      sum += i->number.real;
    else if ( rationalp(i) )
      sum += real(i->number.rational)->number.real;
    else
      raise(runtime_exception("Cannot add real with " + to_s(type_of(i)) + ": " + sprint(i)));
  }

  return real(sum);
}

cons_t* proc_subf(cons_t *p, environment_t*)
{
  real_t sum = 0.0;
  bool first = !nullp(cadr(p)); // length(p)>1

  for ( ; !nullp(p); p = cdr(p) ) {
    cons_t *i = listp(p)? car(p) : p;

    if ( integerp(i) )
      sum -= static_cast<real_t>(i->number.integer);
    else if ( realp(i) )
      sum -= i->number.real;
    else if ( rationalp(i) )
      sum -= real(i->number.rational)->number.real;
    else
      raise(runtime_exception("Cannot add real with " + to_s(type_of(i)) + ": " + sprint(i)));

    if ( first ) {
      sum = -sum;
      first = false;
    }
  }

  return real(sum);
}

cons_t* proc_add(cons_t *p, environment_t* env)
{
  /*
   * Integers have an IDENTITY, so we can do this,
   * but a more correct approach would be to take
   * the value of the FIRST number we find and
   * return that.
   */
  rational_t sum;
  sum.numerator = 0;
  sum.denominator = 1;
  bool exact = true;

  for ( ; !nullp(p); p = cdr(p) ) {
    cons_t *i = listp(p)? car(p) : p;

    if ( integerp(i) ) {
      if ( !i->number.exact ) exact = false;
      sum += i->number.integer;
    } else if ( rationalp(i) ) {
      if ( !i->number.exact ) exact = false;
      sum += i->number.rational;
    } else if ( realp(i) ) {
      // automatically convert; perform rest of computation in floats
      exact = false;
      return proc_addf(cons(real(sum), p), env);
    } else
      raise(runtime_exception(
        "Cannot add integer with " + to_s(type_of(i)) + ": " + sprint(i)));
  }

  return rational(sum, exact);
}

cons_t* proc_sub(cons_t *p, environment_t* env)
{
  /*
   * Integers have an IDENTITY, so we can do this,
   * but a more correct approach would be to take
   * the value of the FIRST number we find and
   * return that.
   */
  rational_t sum;
  sum.numerator = 0;
  sum.denominator = 1;
  bool exact = true;
  bool first = !nullp(cadr(p)); // length(p)>1

  for ( ; !nullp(p); p = cdr(p) ) {
    cons_t *i = listp(p)? car(p) : p;

    if ( integerp(i) ) {
      if ( !i->number.exact ) exact = false;
      sum -= i->number.integer;
    } else if ( rationalp(i) ) {
      if ( !i->number.exact ) exact = false;
      sum -= i->number.rational;
    } else if ( realp(i) ) {
      // perform rest of computation in floats
      return proc_subf(first? p : cons(real(sum), p), env);
    } else
      raise(runtime_exception(
        "Cannot subtract integer with " + to_s(type_of(i)) + ": " + sprint(i)));

    if ( first ) {
      sum.negate();
      first = false;
    }
  }

  return rational(sum, exact);
}

cons_t* proc_divf(cons_t *p, environment_t*)
{
  assert_length(p, 2);

  cons_t *a = car(p);
  cons_t *b = cadr(p);

  real_t x = (type_of(a) == REAL)? a->number.real : a->number.integer;
  real_t y = (type_of(b) == REAL)? b->number.real : b->number.integer;

  // Automatically convert back to int if possible
  real_t q = x / y;
  return iswhole(q)? integer(static_cast<int>(q)) : real(q);
}

cons_t* proc_div(cons_t *p, environment_t *e)
{
  assert_length(p, 2);

  cons_t *a = car(p);
  cons_t *b = cadr(p);

  assert_number(a);
  assert_number(b);

  bool exact = (a->number.exact && b->number.exact);

  if ( zerop(b) )
    raise(runtime_exception(format(
      "Division by zero: %s", sprint(cons(symbol("/"), p)).c_str())));

  if ( type_of(a) == type_of(b) ) {
    if ( integerp(a) ) {
      // division yields integer?
      if ( gcd(a->number.integer, b->number.integer) == 0)
        return integer(a->number.integer / b->number.integer, exact);
      else
        return rational(make_rational(a) /= make_rational(b), exact);
    } else if ( realp(a) )
      return real(a->number.real / b->number.real);
    else if ( rationalp(a) )
      return rational(a->number.rational / b->number.rational, exact);
    else
      raise(runtime_exception(format("Cannot perform division on %s",
        indef_art(to_s(type_of(a))).c_str())));
  }

  bool anyrational = (rationalp(a) || rationalp(b));
  bool anyinteger = (integerp(a) || integerp(b));

  // int/rat or rat/int ==> turn into rational, and not an int
  if ( anyrational && anyinteger )
    return rational(make_rational(a) /= make_rational(b), exact, false);

  // proceed with real division
  return proc_divf(p, e);
}

cons_t* proc_mulf(cons_t *p, environment_t*)
{
  real_t product = 1.0;

  for ( ; !nullp(p); p = cdr(p) ) {
    cons_t *i = listp(p)? car(p) : p;

    if ( integerp(i) )
      product *= static_cast<real_t>(i->number.integer);
    else if ( realp(i) )
      // automatically convert; perform rest of computation in floats
      product *= i->number.real;
    else
      raise(runtime_exception("Cannot multiply integer with " + to_s(type_of(i)) + ": " + sprint(i)));
  }

  return real(product);
}

cons_t* proc_mul(cons_t *p, environment_t *env)
{
  rational_t product;
  product.numerator = 1;
  product.denominator = 1;
  bool exact = true;

  for ( ; !nullp(p); p = cdr(p) ) {
    cons_t *i = listp(p)? car(p) : p;

    if ( integerp(i) ) {
      product *= i->number.integer;
      if ( !i->number.exact ) exact = false;
    } else if ( rationalp(i) ) {
      if ( !i->number.exact ) exact = false;
      product *= i->number.rational;
    } else if ( realp(i) ) {
      // automatically convert; perform rest of computation in floats
      exact = false;
      return proc_mulf(cons(real(product), p), env);
    } else
      raise(runtime_exception("Cannot multiply integer with " + to_s(type_of(i)) + ": " + sprint(i)));
  }

  return rational(product, exact);
}

/*
 * True if number sequence is monotonically increasing.
 */
cons_t* proc_less(cons_t* p, environment_t*)
{
  assert_length_min(p, 2);

  for ( ; !nullp(cdr(p)); p = cdr(p) ) {
    if ( nanp(car(p)) || nanp(cadr(p)) )
      return boolean(false);

    assert_number(car(p));
    assert_number(cadr(p));

    real_t x = integerp(car(p))? car(p)->number.integer :
                                 car(p)->number.real;

    real_t y = integerp(cadr(p))? cadr(p)->number.integer :
                                  cadr(p)->number.real;
    if ( !(x < y) )
      return boolean(false);
  }

  return boolean(true);
}

/*
 * True if number sequence is monotonically decreasing.
 */
cons_t* proc_greater(cons_t* p, environment_t*)
{
  assert_length_min(p, 2);

  for ( ; !nullp(cdr(p)); p = cdr(p) ) {
    if ( nanp(car(p)) || nanp(cadr(p)) )
      return boolean(false);

    assert_number(car(p));
    assert_number(cadr(p));

    real_t x = integerp(car(p))? car(p)->number.integer :
                                 car(p)->number.real;

    real_t y = integerp(cadr(p))? cadr(p)->number.integer :
                                  cadr(p)->number.real;
    if ( !(x > y) )
      return boolean(false);
  }

  return boolean(true);
}

cons_t* proc_number_to_string(cons_t* p, environment_t* e)
{
  assert_length(p, 1, 2);
  assert_number(car(p));

//  int radix = 10;
  if ( !nullp(cadr(p)) ) {
    assert_type(INTEGER, cadr(p));
    //radix = cadr(p)->number.integer;
  }

  // TODO: Implement use of radix
  return proc_to_string(cons(car(p)), e);
}

cons_t* proc_evenp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));
  return boolean(!(car(p)->number.integer & 1));
}

cons_t* proc_oddp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));
  return boolean(car(p)->number.integer & 1);
}

cons_t* proc_negativep(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));
  return boolean(integerp(car(p)) ? car(p)->number.integer < 0 :
                                    car(p)->number.real < 0);
}

cons_t* proc_positivep(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));
  return boolean(integerp(car(p)) ? car(p)->number.integer > 0 :
                                    car(p)->number.real > 0);
}

cons_t* proc_round(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));

  if ( integerp(car(p)) )
    return integer(car(p)->number.integer);
  else if ( rationalp(car(p)) )
    return real(roundf(make_inexact(car(p))->number.real));

  assert_type(REAL, car(p));
  return real(roundf(car(p)->number.real));
}

cons_t* proc_truncate(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));

  if ( integerp(car(p)) )
    return integer(car(p)->number.integer);
  else
    return real(truncf(car(p)->number.real));
}

cons_t* proc_min(cons_t* p, environment_t*)
{
  assert_length_min(p, 1);
  cons_t *min = car(p);

  while ( !nullp(p) ) {
    assert_number(car(p));

    if ( number_to_real(car(p)) < number_to_real(min) )
      min = car(p);

    p = cdr(p);
  }

  return min;
}

cons_t* proc_max(cons_t* p, environment_t*)
{
  assert_length_min(p, 1);
  cons_t *max = car(p);

  while ( !nullp(p) ) {
    assert_number(car(p));

    if ( number_to_real(car(p)) > number_to_real(max) )
      max = car(p);

    p = cdr(p);
  }

  return max;
}

cons_t* proc_expt(cons_t* p, environment_t*)
{
  assert_length(p, 2);

  cons_t *base = car(p),
         *expn = cadr(p);

  assert_number(base);
  assert_number(expn);

  bool exact = integerp(base) && integerp(expn);

  if ( exact ) {
    int a = base->number.integer,
        n = expn->number.integer,
        r = a;

    // Per definition
    if ( n == 0 )
      return integer(1);

    if ( n < 0 )
      raise(runtime_exception("Negative exponents not implemented"));

    // This is a slow version
    // TODO: Implement O(log n) version
    while ( n-- > 1 )
      r *= a;

    return integer(r);
  }

  // Floating point exponentiation
  real_t a = number_to_real(base),
            n = number_to_real(expn),
            r = a;

  if ( n == 0.0 )
    return real(1.0);

  if ( n < 0.0 )
    raise(runtime_exception("Negative exponents not implemented"));

  while ( floor(n) > 1.0 ) {
    r *= a;
    n -= 1.0;
  }

  if ( n > 1.0 )
    raise(runtime_exception("Fractional exponents not supported"));

  // TODO: Compute r^n, where n is in [0..1)
  return real(r);
}

cons_t* proc_modulo(cons_t* p, environment_t*)
{
  assert_length(p, 2);

  cons_t *a = car(p),
         *b = cadr(p);

  assert_type(INTEGER, a);
  assert_type(INTEGER, b);

  if ( b->number.integer == 0 )
    raise(runtime_exception("Division by zero"));

  if ( b->number.integer < 0 )
    raise(runtime_exception("Negative modulus operations not implemented")); // TODO

  return integer(a->number.integer % b->number.integer);
}

cons_t* proc_lcm(cons_t* p, environment_t* e)
{
  switch ( length(p) ) {
  case 0:
    return integer(1);

  case 1:
    assert_type(INTEGER, car(p));
    return integer(abs(car(p)->number.integer));

  case 2: {
    assert_type(INTEGER, cadr(p));

    int a = abs(car(p)->number.integer),
        b = abs(cadr(p)->number.integer);

    return integer(lcm(a, b));
  }

  default: {
    /*
     * We have at least 3 numbers; handle recursively, since
     * lcm(a, b, c) = lcm(lcm(a, b), c)
     */
    cons_t *r = car(p);
    p = cdr(p);

    while ( !nullp(p) ) {
      r = proc_lcm(list(r, car(p)), e);
      p = cdr(p);
    }

    return integer(r->number.integer);
  } }
}

cons_t* proc_nanp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));

  if ( realp(car(p)) )
    return boolean(std::isnan(car(p)->number.real));

  return boolean(false);
}

cons_t* proc_infinitep(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));

  if ( type_of(car(p)) == INTEGER )
    return boolean(false);

  return boolean(std::fpclassify(car(p)->number.real) == FP_INFINITE);
}

cons_t* proc_finitep(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));

  if ( type_of(car(p)) == INTEGER )
    return boolean(true);

  return boolean(std::isfinite(car(p)->number.real));
}

cons_t* proc_gcd(cons_t* p, environment_t* e)
{
  switch ( length(p) ) {
  case 0:
    return integer(0);

  case 1:
    assert_type(INTEGER, car(p));
    return integer(abs(car(p)->number.integer));

  case 2: {
    assert_type(INTEGER, car(p));
    assert_type(INTEGER, cadr(p));

    int a = abs(car(p)->number.integer),
        b = abs(cadr(p)->number.integer);

    return integer(gcd(a, b));
  }

  default: {
    /*
     * We have at least 3 numbers; handle recursively, since
     * gcd(a, b, c) = gcd(gcd(a, b), c)
     */
    cons_t *r = car(p);
    p = cdr(p);

    while ( !nullp(p) ) {
      r = proc_gcd(list(r, car(p)), e);
      p = cdr(p);
    }

    return integer(r->number.integer);
  } }
}

cons_t* proc_exactp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));
  return boolean(car(p)->number.exact == true);
}

cons_t* proc_inexactp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));
  return boolean(car(p)->number.exact == false);
}

cons_t* proc_exact_to_inexact(cons_t* p, environment_t*)
{
  assert_length(p, 1);

  cons_t *q = car(p);
  assert_number(q);
  assert_exact(q);

  cons_t *r = new cons_t();
  *r = *q;
  r->number.exact = false;
  return r;
}

cons_t* proc_exact(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return make_exact(car(p));
}

cons_t* proc_inexact(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return make_inexact(car(p));
}

} // extern "C"
