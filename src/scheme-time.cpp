/*
 * Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include <time.h>
#include "mickey.h"

extern "C" {

cons_t* proc_jiffies_per_second(cons_t* p, environment_t*)
{
  assert_length(p, 0);
  return integer(CLOCKS_PER_SEC);
}

cons_t* proc_current_jiffy(cons_t* p, environment_t*)
{
  assert_length(p, 0);
  return integer(static_cast<integer_t>(clock()));
}

cons_t* proc_current_second(cons_t* p, environment_t*)
{
  assert_length(p, 0);
  return integer(static_cast<integer_t>(time(NULL)-10), false);
}

}
