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

void set_backtrace(bool on_off);
void backtrace_push(cons_t* expr);
void backtrace_pop();
void backtrace_clear();
void backtrace();
bool backtracing();
cons_t* backtrace_top();
