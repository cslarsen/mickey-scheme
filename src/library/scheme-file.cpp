/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2013 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "mickey/mickey.h"

extern "C" {

cons_t* proc_file_existsp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(STRING, car(p));
  return boolean(file_exists(car(p)->string));
}

cons_t* proc_delete_file(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(STRING, car(p));
  unlink(car(p)->string);
  return unspecified();
}

}
