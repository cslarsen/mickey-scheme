/*
 * UNIX wait for Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under the LGPL 2.1; see LICENSE
 */

#include <signal.h>
#include "mickey/mickey.h"

extern "C" cons_t* proc_kill(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(INTEGER, car(p));
  assert_type(INTEGER, cadr(p));

  pid_t pid = car(p)->number.integer;
  int sig = cadr(p)->number.integer;

  return integer(kill(pid, sig));
}
