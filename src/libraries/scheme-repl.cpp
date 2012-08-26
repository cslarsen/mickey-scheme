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

extern "C" {

cons_t* proc_interaction_environment(cons_t *p, environment_t *e)
{
  assert_length(p, 0);

  // first level
  if ( (e = e->outer) == NULL )
    raise(runtime_exception("Found no parent environment"));

  // second level
  if ( (e = e->outer) == NULL )
    raise(runtime_exception("Found no parent environment"));

  /*
   * The "levels" above are deeply tied into the specifics in
   * how we have implemented function calls in Mickey Scheme.
   */
  return environment(e);
}

}
