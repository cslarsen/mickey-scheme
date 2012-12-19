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

#ifndef INC_MICKEY_NUMBER_H
#define INC_MICKEY_NUMBER_H

#include "integer_t.h"
#include "rational_t.h"
#include "real_t.h"

/*
 * Non-complex number with exactness
 */
struct number_t {
  bool exact;
  union {
    integer_t integer;
    real_t real;
    rational_t rational;
  };
};

#endif
