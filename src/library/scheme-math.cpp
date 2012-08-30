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

#include <math.h>
#include "mickey.h"

/*
 * All <math.h> procedures have identical signatures,
 * so create a macro to, in darkness, bind them.
 */
#define MAKE_PROC(name, math_fun)                       \
  cons_t* name(cons_t* p, environment_t*)               \
  {                                                     \
    assert_length(p, 1);                                \
    assert_number(car(p));                              \
    return decimal(math_fun(number_to_double(car(p)))); \
  }

extern "C" {

MAKE_PROC(proc_exp, exp);
MAKE_PROC(proc_log, log);
MAKE_PROC(proc_sin, sin);
MAKE_PROC(proc_cos, cos);
MAKE_PROC(proc_tan, tan);
MAKE_PROC(proc_asin, asin);
MAKE_PROC(proc_acos, acos);
MAKE_PROC(proc_sqrt, sqrt);
MAKE_PROC(proc_ceil, ceil);
MAKE_PROC(proc_floor, floor);

cons_t* proc_atan(cons_t* p, environment_t*)
{
  if ( length(p) == 1 ) {
    assert_number(car(p));
    return decimal(atan(number_to_double(car(p))));
  } else if ( length(p) == 2 ) {
    raise(runtime_exception("Two-argument atan is not (yet) supported"));
  } else
    raise(runtime_exception("Function atan requires one or two arguments"));

  // make explicit return, so that the compiler is happy
  return list(NULL);
}

named_function_t exports_math[] = {
  {"exp", proc_exp, false},
  {"log", proc_log, false},
  {"sin", proc_sin, false},
  {"cos", proc_cos, false},
  {"tan", proc_tan, false},
  {"asin", proc_asin, false},
  {"acos", proc_acos, false},
  {"atan", proc_atan, false},
  {"sqrt", proc_sqrt, false},
  {"ceiling", proc_ceil, false},
  {"floor", proc_floor, false},
  {NULL, NULL, false}
};

}
