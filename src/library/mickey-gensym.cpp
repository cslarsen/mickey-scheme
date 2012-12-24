/*
 * Mickey Scheme
 *
 * Copyright (C) 2012 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include "mickey.h"

extern "C" {

static integer_t number = 0;

cons_t* proc_gensym(cons_t*, environment_t*)
{
  return string(format("gensym-%d", number++).c_str());
}

};
