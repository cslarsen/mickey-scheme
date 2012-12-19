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

#ifndef INC_MICKEY_CONS_H
#define INC_MICKEY_CONS_H

#include "types/bytevector_t.h"
#include "types/character_t.h"
#include "types/closure_t.h"
#include "types/continuation_t.h"
#include "types/dict_t.h"
#include "types/environment_t.h"
#include "types/integer_t.h"
#include "types/lambda_t.h"
#include "types/number_t.h"
#include "types/pointer_t.h"
#include "types/port_t.h"
#include "types/radix_t.h"
#include "types/rational_t.h"
#include "types/real_t.h"
#include "types/symbol_t.h"
#include "types/syntax_t.h"
#include "types/type_t.h"
#include "types/vector_t.h"

/*
 * A variant variable.  Called a cons-cell, but that's incorrect.
 *
 * TODO: To cons_t, Add `marked` (for GC) and `mutable/immutable` (per spec)
 */
struct cons_t {
  type_t type;
  union {
    bool boolean;
    character_t character;
    number_t number;
    struct { cons_t *car, *cdr; }; // pair
    closure_t* closure;
    syntax_t* syntax;
    const symbol_t* symbol;
    const char* string;
    vector_t* vector;
    bytevector_t* bytevector;
    continuation_t* continuation;
    port_t* port;
    environment_t *environment; // first-class environments
    pointer_t *pointer;
  };
};

#endif
