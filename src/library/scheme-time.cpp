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
#include "mickey/mickey.h"

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
  /*
   * TODO: This is incorrect. We're supposed to provide TAI seconds here, see
   * https://en.wikipedia.org/wiki/International_Atomic_Time which means if the
   * system date is, for example, 1961, the offset is different.
   *
   * See also http://cr.yp.to/proto/utctai.html
   */
  assert_length(p, 0);

  // TODO: The offset changes depending on a table of leap seconds added for
  // different date ranges.
  const double offset = +35;
  return integer(static_cast<integer_t>(time(NULL) + offset), false);
}

}
