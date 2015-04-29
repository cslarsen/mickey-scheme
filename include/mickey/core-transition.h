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

#ifndef INC_MICKEY_CORE_TRANS_H
#define INC_MICKEY_CORE_TRANS_H

#include "mickey/cons.h"

extern "C" {

cons_t* proc_and(cons_t* p, environment_t* e);
cons_t* proc_begin(cons_t* p, environment_t* e);
cons_t* proc_case(cons_t *p, environment_t* e);
cons_t* proc_cond(cons_t* p, environment_t* e);
cons_t* proc_define(cons_t *p, environment_t *e);
cons_t* proc_define_syntax(cons_t *p, environment_t *e);
cons_t* proc_do(cons_t* p, environment_t* e);
cons_t* proc_let(cons_t* p, environment_t* e);
cons_t* proc_letrec(cons_t* p, environment_t* e);
cons_t* proc_letstar(cons_t* p, environment_t* e);
cons_t* proc_or(cons_t* p, environment_t* e);
cons_t* proc_set_car(cons_t* p, environment_t* e);
cons_t* proc_set_cdr(cons_t* p, environment_t* e);
cons_t* proc_vector(cons_t* p, environment_t* e);
cons_t* proc_define_macro(cons_t*, environment_t*);

}

#endif
