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

#include "mickey/mickey.h"

cons_t* make_closure(cons_t* args, cons_t* body, environment_t* e)
{
  cons_t* r = gc_alloc_cons(CLOSURE);
  r->closure = gc_alloc_closure();
  r->closure->function = NULL;
  r->closure->environment = e;
  r->closure->args = args;
  r->closure->body = cons(symbol("begin"), body);
  return r;
}
