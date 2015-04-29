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

#include "mickey/evlis.h"
#include "mickey/primitives.h"
#include "mickey/eval.h"
#include "mickey/print.h"

cons_t* evlis(cons_t* p, environment_t* e)
{
  cons_t *r = list();

  /*
   * We use a tail pointer `t´ to avoid using the slow append()
   */
  for ( cons_t *t = r; pairp(p); p = cdr(p) ) {
    t->car = eval(car(p), e);
    t->cdr = cons(nil());
    t = cdr(t);
  }

  return r;
}
