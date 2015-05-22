/*
 * Mickey Scheme
 *
 * Copyright (C) 2015 Christian Stigen Larsen <csl@csl.name>
 * http://csl.name
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#ifndef INC_MICKEY_LIBRARY_H
#define INC_MICKEY_LIBRARY_H

#include "mickey/mickey.h"
#include "mickey/cons.h"
#include "mickey/primitives.h"
#include "mickey/types/environment_t.h"

struct library_t {
  cons_t *name;
  environment_t *exports; // exported definitions
  environment_t *internals; // imported libs/defs

  library_t() :
    name(nil()),
    exports(null_environment()),
    internals(exports->extend())
  {
  }
};

#endif
