/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-201 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#ifndef INC_MICKEY_PRINT_H
#define INC_MICKEY_PRINT_H

#include <string>
#include "cons.h"

// print escaped data values
std::string sprint(const cons_t* p);

// print data values "as-is", i.e. do not escape stuff
std::string print(const cons_t* p);

#endif
