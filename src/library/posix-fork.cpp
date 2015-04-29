/*
 * UNIX fork for Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under the LGPL 2.1; see LICENSE
 */

#include <unistd.h>
#include "mickey/mickey.h"

extern "C" cons_t* proc_fork(cons_t* p, environment_t*)
{
  assert_length(p, 0);
  return integer(fork());
}
