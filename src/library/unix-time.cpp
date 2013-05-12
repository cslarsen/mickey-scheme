/*
 * UNIX wait for Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under the LGPL 2.1; see LICENSE
 */

#include <time.h>
#include <errno.h>
#include <string.h>
#include "mickey.h"

static const char* id_time_t = "time_t*";

extern "C" cons_t* proc_time(cons_t* p, environment_t*)
{
  assert_length(p, 0, 1);

  time_t *t = new time_t(0);

  if ( length(p) == 1 ) {
    assert_type(INTEGER, car(p));
    *t = car(p)->number.integer;
  } else {
    if ( time(t) == static_cast<time_t>(-1) )
      raise(runtime_exception(format(
        "time(): %s", strerror(errno))));
  }

  return pointer(id_time_t, t);
}

extern "C" cons_t* proc_time_value(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer(id_time_t, car(p));
  time_t *t = static_cast<time_t*>(car(p)->pointer->value);
  return integer(static_cast<long>(*t));
}

extern "C" cons_t* proc_ctime(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer(id_time_t, car(p));
  time_t *t = static_cast<time_t*>(car(p)->pointer->value);

  char buf[32];
  memset(buf, '\0', sizeof(buf));

  ctime_r(t, buf);
  return string(buf);
}
