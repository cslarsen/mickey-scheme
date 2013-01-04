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

extern "C" cons_t* proc_gethostname(cons_t* p, environment_t*)
{
  assert_length(p, 0);

  size_t len = 1+sysconf(_SC_HOST_NAME_MAX);
  char *s = static_cast<char*>(malloc(sizeof(char)*len));
  gethostname(s, len);
  cons_t *r = string(s);
  free(s);

  return r;
}

extern "C" cons_t* proc_usleep(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));

  int usecs = car(p)->number.integer;

  if ( usecs < 0 )
    raise(runtime_exception("usleep doesn't like negative time"));

  // TODO: If it returns -1, lookup error and return it
  return integer(usleep(usecs));
}
