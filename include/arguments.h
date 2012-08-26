/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2012 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#ifndef INC_MICKEY_ARGUMENTS_H
#define INC_MICKEY_ARGUMENTS_H

#include "cons.h"

/*
 * Return number of required arguments, excluding
 * "rest" arguments, e.g. "(x y z . rest)" or "rest".
 */
size_t arg_length(cons_t*);

bool has_rest_args(cons_t*);

#endif
