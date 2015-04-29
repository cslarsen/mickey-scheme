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

#ifndef INC_MICKEY_COND_EXPAND_H
#define INC_MICKEY_COND_EXPAND_H

#include "mickey/cons.h"

extern "C" cons_t* cond_expand(const cons_t* p, environment_t*);

#endif
