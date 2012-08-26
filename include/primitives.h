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

#ifndef INC_MICKEY_PRIMOPS_H
#define INC_MICKEY_PRIMOPS_H

#include "cons.h"
#include "tokenizer.h"

type_t type_of(const cons_t* p);
size_t length(const cons_t*);

cons_t* cons(const cons_t* head, const cons_t* tail = NULL);
cons_t* list(const cons_t* head = NULL, const cons_t* tail = NULL);
cons_t* append(cons_t*, cons_t*);
cons_t* splice(cons_t*, cons_t*);

/*
 * Splice one list directly into the other and
 * return the end of the target list afterwards.
 */
cons_t* splice_into(cons_t *from, cons_t *to);

cons_t* symbol(const char*, environment_t*);
cons_t* integer(int);
cons_t* boolean(bool);
cons_t* character(char);
cons_t* decimal(decimal_t);
cons_t* string(const char*);
cons_t* vector(cons_t*, size_t size=0, cons_t* fill=NULL);
cons_t* bytevector(size_t size=0, const uint8_t *fill=NULL);
cons_t* bytevector(const std::vector<uint8_t>& p);
cons_t* port(port_t*);
cons_t* environment(environment_t*);
cons_t* pointer(pointer_t*);
cons_t* closure(lambda_t, environment_t*, bool syntactic = false);
cons_t* nil();

cons_t* caaar(const cons_t*);
cons_t* caaddr(const cons_t*);
cons_t* caadr(const cons_t*);
cons_t* caar(const cons_t*);
cons_t* cadaar(const cons_t*);
cons_t* cadar(const cons_t*);
cons_t* caddar(const cons_t*);
cons_t* caddr(const cons_t*);
cons_t* cadr(const cons_t*);
cons_t* car(const cons_t*);
cons_t* cdaddr(const cons_t*);
cons_t* cdar(const cons_t*);
cons_t* cdddr(const cons_t*);
cons_t* cddddr(const cons_t*);
cons_t* caddddr(const cons_t*);
cons_t* cadddr(const cons_t*);
cons_t* cddr(const cons_t*);
cons_t* cdr(const cons_t*);

bool symbolp(const cons_t*);
bool atomp(const cons_t*);
bool integerp(const cons_t*);
bool decimalp(const cons_t*);
bool nullp(const cons_t*);
bool pairp(const cons_t*);
bool listp(const cons_t*);
bool properlistp(const cons_t*);
bool stringp(const cons_t*);
bool closurep(const cons_t*);
bool syntaxp(const cons_t*);
bool booleanp(const cons_t*);
bool vectorp(const cons_t*);
bool bytevectorp(const cons_t*);
bool portp(const cons_t*);
bool environmentp(const cons_t*);
bool charp(const cons_t*);
bool numberp(const cons_t*);
bool eqp(const cons_t*, const cons_t*);
bool equalp(const cons_t*, const cons_t*);
bool eqvp(const cons_t*, const cons_t*);
bool emptylistp(const cons_t*);

bool not_p(const cons_t*);
bool and_p(const cons_t*);
bool or_p(const cons_t*);
bool xor_p(const cons_t*);

double number_to_double(const cons_t*);
decimal_t number_to_float(const cons_t*);
bool iswhole(decimal_t);

int gcd(int a, int b);
int lcm(int a, int b);
cons_t* nil_coalesce(cons_t* p);
bool boolean_false(cons_t* p);

/*
 * If argument is a symbol, return its name.
 * If not, return empty string.
 */
const std::string& symbol_name(const cons_t*);

/*
 * Returns blank environment.
 *
 * TODO: Move this to own environment.h / environment.cpp
 */
environment_t* null_environment(int version = 7);

/*
 * Returns blank environment with only one
 * definition: import.
 *
 * TODO: Move this to own environment.h / environment.cpp
 */
environment_t* null_import_environment();

#endif
