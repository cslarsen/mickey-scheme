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

#include <map>
#include "mickey/cons.h"
#include "mickey/types/symbol_t.h"

/*
 * Symbol tables maps strings to values.
 */
typedef std::map<symbol_t, cons_t*> dict_t;
