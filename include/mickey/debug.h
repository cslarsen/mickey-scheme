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

#ifndef INC_MICKEY_DEBUG_H
#define INC_MICKEY_DEBUG_H

#include "mickey/cons.h"

/*
 * DPRINT(x) -- debug print cons_t* type.
 */

#ifdef DEBUG
#define DPRINT(x) { \
  fprintf(stderr, "%s:%d " #x ": %s\n", \
      __FILE__, __LINE__, sprint(x).c_str()); \
  fflush(stderr); }
#else
#define DPRINT(x)
#endif

#endif
