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

#include "mickey/mickey.h"

extern "C" {

// Some selected procedures

#define DECLARE(name) cons_t* name (cons_t*, environment_t*)

DECLARE( proc_begin );
DECLARE( proc_case );
DECLARE( proc_cond );
DECLARE( proc_define );
DECLARE( proc_define_syntax );
DECLARE( proc_do );
DECLARE( proc_eqnump );
DECLARE( proc_eqp );
DECLARE( proc_equalp );
DECLARE( proc_eqvp );
DECLARE( proc_let );
DECLARE( proc_letrec );
DECLARE( proc_letstar );
DECLARE( proc_map );
DECLARE( proc_set_car );
DECLARE( proc_set_cdr );
DECLARE( proc_to_string );
DECLARE( proc_vector );
DECLARE( proc_zerop );

}
