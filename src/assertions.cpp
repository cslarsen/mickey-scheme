/*
 * Mickey Scheme
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *                                                          
 */

#include "assertions.h"
#include "primitives.h"
#include "print.h"
#include "util.h"
#include "circular.h"
#include "exceptions.h"

void assert_length(const cons_t* p, const size_t e)
{
  const size_t l = length(const_cast<cons_t*>(p));

  if ( e != l )
    raise(runtime_exception(format(
      "Function expects exactly %lu parameters but got %lu: `%s´",
        e, l, sprint(p).c_str())));
}

void assert_length(const cons_t* p, const size_t min, const size_t max)
{
  const size_t l = length(const_cast<cons_t*>(p));

  if ( l<min || l>max )
    raise(runtime_exception(format(
      "Function expects from %lu to %lu parameters but got %lu: `%s´",
        min, max, l, sprint(p).c_str())));
}

void assert_length_min(const cons_t* p, const size_t min)
{
  const size_t l = length(const_cast<cons_t*>(p));

  if ( l<min )
    raise(runtime_exception(format(
      "Function expects at least %lu parameters got %lu: `%s´",
        min, l, sprint(p).c_str())));
}

void assert_type(const enum type_t type, const cons_t* p)
{
  if ( p == NULL )
    raise(runtime_exception(format(
      "Function expected %s but got NULL",
        indef_art(to_s(type)).c_str())));

  bool error = false;

  if ( type != PAIR )
    /*
     * If expected type is NOT a pair, then types must match.
     */
    error = type_of(p) != type;
  else
    /*
     * If expected type IS a pair, then we can submit either
     * a LIST or a VECTOR to a function.
     */
    error = !(type_of(p) == VECTOR || type_of(p) == PAIR);

  if ( error ) {
    raise(runtime_exception(format("Function expected %s but got %s: `%s´",
      indef_art(to_s(type)).c_str(),
      indef_art(to_s(type_of(p))).c_str(),
      sprint(p).c_str())));
  }
}

void assert_number(const cons_t* p)
{
  if ( !numberp(p) )
    raise(runtime_exception(format("Function expected a number but got a %s: `%s´",
      to_s(type_of(p)).c_str(),
      sprint(p).c_str())));
}

void assert_noncyclic(const cons_t* p)
{
  if ( circularp(p) )
    raise(runtime_exception("List contains cycles"));
}

void assert_proper_list(const cons_t* p)
{
  if ( !properlistp(p) )
    raise(runtime_exception(
      format("Not a proper list: %s", sprint(p).c_str())));
}

void assert_pointer(const char* tag, const cons_t* p)
{
  if ( type_of(p) != POINTER )
    raise(runtime_exception(format(
      "Function expected a pointer but got a %s: %s",
      to_s(type_of(p)).c_str(),
      sprint(p).c_str())));

  if ( strcmp(tag, p->pointer->tag) != 0 )
    raise(runtime_exception(format(
      "Function expected a pointer with tag '%s' but got '%s': %s",
        tag, p->pointer->tag, sprint(p).c_str())));
}
