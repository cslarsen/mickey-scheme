/*
 * Part of Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under the LGPL 2.1; see LICENSE
 *
 */

#include "mickey.h"

extern "C" {

cons_t* proc_environment(cons_t* p, environment_t*)
{
  assert_length_min(p, 1);

  environment_t *out = null_environment(7);

  // Handle import sets
  for ( ; !nullp(p); p = cdr(p) ) {
    environment_t *impenv = import_set(car(p));
    merge(out, impenv);
    impenv->outer = out;
  }

  return environment(out);
}

}
