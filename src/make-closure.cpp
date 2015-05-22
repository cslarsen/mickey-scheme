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
  closure_t *c = gc_alloc_closure();
  c->function = NULL;
  c->environment = e;
  c->args = args;
  c->body = cons(symbol("begin"), body);

  cons_t* r = gc_alloc_cons();
  r->type = CLOSURE;
  r->closure = c;
  return r;
}

