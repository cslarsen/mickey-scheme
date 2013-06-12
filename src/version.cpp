/*
 * Part of Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under the LGPL 2.1; see LICENSE
 *
 */

#include "mickey.h"

extern "C" cons_t* proc_implementation_name(cons_t* p, environment_t*)
{
  assert_length(p, 0);
  return string(MICKEY_NAME);
}

extern "C" cons_t* proc_implementation_version(cons_t* p, environment_t*)
{
  assert_length(p, 0);
  return string(format("%d.%d", MICKEY_VERSION_MAJOR,
                                MICKEY_VERSION_MINOR).c_str());
}
