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

#include "mickey.h"

cons_t* make_closure(cons_t* args, cons_t* body, environment_t* e)
{
  // Wrap body in begin-block (or else our poor evaluator will execute
  // expressions backwards!)... this is of course an indication that
  // something else is terribly wrong! :-)
  body = cons(symbol("begin", e), body);

  // This is a hack -> will shadow params with these magic names (FIXME)
  e->define(ARGS, args);
  e->define(BODY, body);

  closure_t *c = new closure_t();
  c->function = call_lambda;
  c->environment = e;

  cons_t* r = new cons_t();
  r->type = CLOSURE;
  r->closure = c;
  return r;
}

