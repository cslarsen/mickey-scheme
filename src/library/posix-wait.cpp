/*
 * UNIX wait for Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under the LGPL 2.1; see LICENSE
 */

#include <sys/wait.h>
#include "mickey/mickey.h"

extern "C" cons_t* proc_wait(cons_t* p, environment_t*)
{
  assert_length(p, 0);

  int status = 0;
  pid_t pid = wait(&status);

  return list(integer(pid), integer(status));
}

extern "C" cons_t* proc_exitedp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));
  return boolean(WIFEXITED(car(p)->number.integer) != 0);
}

extern "C" cons_t* proc_signaledp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));
  return boolean(WIFSIGNALED(car(p)->number.integer) != 0);
}

extern "C" cons_t* proc_stoppedp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));
  return boolean(WIFSTOPPED(car(p)->number.integer) != 0);
}

extern "C" cons_t* proc_exitstatus(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));

  int status = car(p)->number.integer;
  return integer(WEXITSTATUS(status));
}

extern "C" cons_t* proc_termsig(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));

  int status = car(p)->number.integer;
  return integer(WTERMSIG(status));
}

extern "C" cons_t* proc_coredumpp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));

  int status = car(p)->number.integer;
  return boolean(WCOREDUMP(status) != 0);
}

extern "C" cons_t* proc_stopsig(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));

  int status = car(p)->number.integer;
  return integer(WSTOPSIG(status));
}
