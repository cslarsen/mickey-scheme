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

#include "lambda_t.h"

/*
 * A closure consists of a function and an environment.  A syntax flag tells
 * whether to evaluate parameters before function invocation.
 *
 * lambda_t is a C function with a given signature.  For code stored as
 * source code, we need to store the body and argument list.
 */
struct closure_t {
  lambda_t function;
  environment_t* environment;
  bool syntactic;
  cons_t *body, *args;
};
