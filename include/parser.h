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

#ifndef INC_MICKEY_PARSER_H
#define INC_MICKEY_PARSER_H

#include "cons.h"
#include "primitives.h"

struct program_t {
  environment_t *globals;
  cons_t *root;
  long int parens;
};

program_t* parse(const std::string& program, environment_t *env);
cons_t* parse_string(const char*);

#endif
