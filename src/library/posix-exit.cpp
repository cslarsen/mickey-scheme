/*
 * UNIX wait for Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under the LGPL 2.1; see LICENSE
 */

#include <stdlib.h>
#include "mickey.h"

extern "C" cons_t* proc_exit(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));

  int status = car(p)->number.integer;
  exit(status);

  return nil();
}
