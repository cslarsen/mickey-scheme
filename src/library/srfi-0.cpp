/*
 * Mickey Scheme
 *
 * Copyright (C) 2012-2013 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include "mickey/mickey.h"
#include "mickey/cond-expand.h"

extern "C" cons_t* srfi0_cond_expand(const cons_t* p, environment_t* e)
{
  return eval(cond_expand(p, e), e);
}
