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

#ifndef INC_MICKEY_UTIL_H
#define INC_MICKEY_UTIL_H

#include <string>
#include "cons.h"

char* copy_str(const char* s);
char* trimr(char* s);

int empty(const char*);
bool char_in(char ch, const char* s);

std::string encode_str(const char* s);

// Prefix string with indefinite article (i.e., "a" or "an")
std::string indef_art(const std::string&);

cons_t* deep_copy(const cons_t*);

std::string sbasename(const std::string&);
integer_t& min(integer_t& a, integer_t& b);

#endif
