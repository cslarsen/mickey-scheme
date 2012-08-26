/*
 * Mickey Scheme
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *                                                          
 */

#include "cons.h"

void assert_length(const cons_t*, const size_t);
void assert_length(const cons_t*, const size_t min, const size_t max);
void assert_length_min(const cons_t*, const size_t min);
void assert_type(const enum type_t, const cons_t*);
void assert_number(const cons_t*);
void assert_noncyclic(const cons_t*);
void assert_proper_list(const cons_t*);
void assert_pointer(const char*, const cons_t*);
