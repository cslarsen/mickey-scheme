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

#include "mickey/apply.h"
#include "mickey/eval.h"

/*
 * TODO: Add environment argument, and do this
 *        - eval returns expr + environment
 *        - apply gobbles expr + environment, returns environment
 *        - eval gobbles up this new environment, etc
 */
cons_t* apply(closure_t* f, cons_t *args, environment_t *env)
{
  return f->function?
    f->function(args, env) :
    call_lambda(args, f, env);
}
