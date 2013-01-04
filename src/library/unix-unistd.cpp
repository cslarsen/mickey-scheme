/*
 * UNIX unistd.h for Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under the LGPL 2.1; see LICENSE
 */

#include <signal.h>
#include "mickey.h"

extern "C" cons_t* proc_getpid(cons_t* p, environment_t*)
{
  assert_length(p, 0);
  return integer(getpid());
}

extern "C" cons_t* proc_getppid(cons_t* p, environment_t*)
{
  assert_length(p, 0);
  return integer(getppid());
}
