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

struct program_t {
  environment_t *globals;
  cons_t *root;
  long int parens;
};

/*
 * Parsers
 */
cons_t* parse_string(const char*);
program_t* parse(const std::string& program, environment_t *env);

/*
 * Predicates
 */
bool isatom(const char*);
bool isbool(const char*);
bool isbytevector(const char*);
bool ischar(const char*);
bool isfloat(const char*);
bool ishex(const char*);
bool isinteger(const char*);
bool isrational(const char*);
bool isquasiquote(const char*);
bool issinglequote(const char*);
bool isstring(const char*);
bool isunquote(const char*);
bool isunquote_splicing(const char*);
bool isvector(const char*);
bool isvowel(char);
int isdot(int s);
int ishex(int s);

/*
 * Conversion from text
 */
bool to_b(const char*);
char literal_to_char(const char*);
char to_char(const char*);
decimal_t to_f(const char*);
enum type_t to_type_t(const char*);
int to_i(const char*);
rational_t to_r(const char*);

/*
 * Pattern matchers
 */
int all(const char* s, int (*check)(int));
int count(const char* s, int (*check)(int));

#endif
