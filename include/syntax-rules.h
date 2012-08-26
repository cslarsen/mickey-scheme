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

#include "cons.h"

#ifndef INC_SYNTAX_RULES_H
#define INC_SYNTAX_RULES_H

extern "C" {

cons_t* make_syntax(cons_t* body, environment_t* e);
cons_t* syntax_expand(cons_t *macro, cons_t *code, environment_t*);

}

#endif
