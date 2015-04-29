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
#include "mickey/library/scheme-base.h"

extern "C" {

cons_t* proc_symbolp(cons_t* p, environment_t*)
{
  return boolean(symbolp(car(p)));
}

cons_t* proc_integerp(cons_t* p, environment_t*)
{
  /*
   * Note that reals like 3.0 should be considered
   * integers.
   */
  if ( realp(car(p)) ) {
    real_t n = car(p)->number.real;
    return boolean((real_t)((int)n) == n);
  }

  return boolean(integerp(car(p)));
}

cons_t* proc_rationalp(cons_t* p, environment_t*)
{
  assert_length(p, 1);


  /*
   * All integers can be written as rationals
   */
  if ( integerp(car(p)) )
    return boolean(true);

  /*
   * All finite reals can be written as rationals
   */
  if ( realp(car(p)) )
    return boolean(true);

  return boolean(rationalp(car(p)));
}

cons_t* proc_realp(cons_t* p, environment_t*)
{
  /*
   * All integers can also be considered reals.
   */
  return boolean(
    realp(car(p)) ||
    integerp(car(p)));
}

cons_t* proc_nullp(cons_t* p, environment_t*)
{
  return boolean(nullp(car(p)));
}

cons_t* proc_pairp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(pairp(car(p)));
}

cons_t* proc_portp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(portp(car(p)));
}

cons_t* proc_port_openp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PORT, car(p));
  return boolean(car(p)->port->isopen());
}

cons_t* proc_input_portp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PORT, car(p));
  return boolean(car(p)->port->isreadable());
}

cons_t* proc_output_portp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PORT, car(p));
  return boolean(car(p)->port->iswritable());
}

cons_t* proc_textual_portp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PORT, car(p));
  return boolean(car(p)->port->istextual());
}

cons_t* proc_binary_portp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PORT, car(p));
  return boolean(car(p)->port->isbinary());
}

cons_t* proc_listp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(properlistp(car(p)));
}

cons_t* proc_stringp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(stringp(car(p)));
}

cons_t* proc_procedurep(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(closurep(car(p)));
}

cons_t* proc_vectorp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(vectorp(car(p)));
}

cons_t* proc_bytevectorp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(bytevectorp(car(p)));
}

cons_t* proc_charp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(charp(car(p)));
}

cons_t* proc_booleanp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(booleanp(car(p)));
}

cons_t* proc_eqp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  return boolean(eqp(car(p), cadr(p)));
}

cons_t* proc_eqvp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  return boolean(eqvp(car(p), cadr(p)));
}

cons_t* proc_equalp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  return boolean(equalp(car(p), cadr(p)));
}

cons_t* proc_eqnump(cons_t* p, environment_t*)
{
  assert_length_min(p, 2);

  for ( ; !nullp(cdr(p)); p = cdr(p) ) {
    if ( nanp(car(p)) || nanp(cadr(p)) )
      return boolean(false);

    assert_number(car(p));
    assert_number(cadr(p));

    cons_t *l = car(p),
           *r = cadr(p);

    if ( realp(l) || realp(r) ) {
      if ( number_to_real(l) == number_to_real(r) )
        continue;

      return boolean(false);
    }

    if ( rationalp(l) && rationalp(r) ) {
      if ( l->number.rational.numerator == r->number.rational.numerator &&
           l->number.rational.denominator == r->number.rational.denominator )
        continue;

      return boolean(false);
    }

    if ( integerp(l) && integerp(r) ) {
      if ( l->number.integer == r->number.integer )
        continue;
      return boolean(false);
    }

    /*
     * To compare fraction with integer:
     * a/b == c
     * a == c*b
     *
     * So just see if a == c*b. There is a potential for arithmetic overflow
     * during this, so please add detection for that (TODO).
     *
     * In case of overflow (it can be detected, e.g. if c*b<b and c*b<c,
     * then c*b has overflown), then try to see if a is divisible by both c
     * and b, e.g. a%c==0 && a&b==0.
     *
     */
    if ( rationalp(l) && integerp(r) ) {
      if ( l->number.rational.numerator ==
           l->number.rational.denominator * r->number.integer )
        continue;
      return boolean(false);
    }
    if ( rationalp(r) && integerp(l) ) {
      if ( r->number.rational.numerator ==
           r->number.rational.denominator * l->number.integer )
        continue;
      return boolean(false);
    }

    std::string msg = "Don't know how to perform equal number comparison "
      "with types " + sprint(l);
    msg += " and " + sprint(r);
    raise(runtime_exception(msg));
  }

  return boolean(true);
}

cons_t* proc_char_ltep(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(CHAR, car(p));
  assert_type(CHAR, cadr(p));
  return boolean(car(p)->character <= cadr(p)->character);
}

cons_t* proc_char_ltp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(CHAR, car(p));
  assert_type(CHAR, cadr(p));
  return boolean(car(p)->character < cadr(p)->character);
}

cons_t* proc_char_eqp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(CHAR, car(p));
  assert_type(CHAR, cadr(p));
  return boolean(car(p)->character == cadr(p)->character);
}

cons_t* proc_char_gtp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(CHAR, car(p));
  assert_type(CHAR, cadr(p));
  return boolean(car(p)->character > cadr(p)->character);
}

cons_t* proc_char_gtep(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(CHAR, car(p));
  assert_type(CHAR, cadr(p));
  return boolean(car(p)->character >= cadr(p)->character);
}

cons_t* proc_string_ltp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(STRING, car(p));
  assert_type(STRING, cadr(p));
  return boolean(strcmp(car(p)->string, cadr(p)->string) < 0);
}

cons_t* proc_string_ltep(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(STRING, car(p));
  assert_type(STRING, cadr(p));
  return boolean(strcmp(car(p)->string, cadr(p)->string) <= 0);
}

cons_t* proc_string_eqp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(STRING, car(p));
  assert_type(STRING, cadr(p));
  return boolean(strcmp(car(p)->string, cadr(p)->string) == 0);
}

cons_t* proc_string_gtep(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(STRING, car(p));
  assert_type(STRING, cadr(p));
  return boolean(strcmp(car(p)->string, cadr(p)->string) >= 0);
}

cons_t* proc_string_gtp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(STRING, car(p));
  assert_type(STRING, cadr(p));
  return boolean(strcmp(car(p)->string, cadr(p)->string) > 0);
}

cons_t* proc_eof_objectp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(type_of(car(p)) == POINTER &&
      !strcmp(car(p)->pointer->tag, "eof-object"));
}

} // extern "C"
