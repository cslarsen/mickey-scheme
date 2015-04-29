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

#ifndef INC_MICKEY_REAL_T_H
#define INC_MICKEY_REAL_T_H

typedef double real_t;


bool is_nan(const real_t&);
bool is_neg_inf(const real_t&);
bool is_pos_inf(const real_t&);
real_t make_nan();
real_t minus_infinity();
real_t plus_infinity();
#endif
