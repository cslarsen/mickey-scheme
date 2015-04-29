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
#include "mickey/cons.h"

// print escaped data values
std::string sprint(const cons_t* p);

// print data values "as-is", i.e. do not escape stuff
std::string print(const cons_t* p);

/*
 * C-style format string
 */
std::string format(const char *fmt, ...);

/*
 * Convert to lowercase
 */
std::string tolower(const std::string& s);

/*
 * COnvert to uppercase
 */
std::string toupper(const std::string& s);

/*
 * Functions to converting data types to string.
 */
std::string to_s(bool f);
std::string to_s(bytevector_t*);
std::string to_s(char, bool);
std::string to_s(closure_t*);
std::string to_s(continuation_t*);
std::string to_s(real_t n);
std::string to_s(enum type_t);
std::string to_s(environment_t*);
std::string to_s(const integer_t& n);
std::string to_s(rational_t n);
std::string to_s(port_t*);
std::string to_s(struct cons_t *p);;
std::string to_s(vector_t*);

std::string sprint(const bytevector_t*, std::string&, bool);
std::string sprint(const environment_t*, std::string&, bool);
std::string sprint(const pointer_t*, std::string&, bool);
std::string sprint(const port_t*, std::string&, bool);
std::string sprint(const vector_t*, std::string&, bool);

#endif
