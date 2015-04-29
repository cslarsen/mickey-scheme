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

#include "mickey/types/bytevector_t.h"
#include "mickey/types/character_t.h"
#include "mickey/types/closure_t.h"
#include "mickey/types/continuation_t.h"
#include "mickey/types/dict_t.h"
#include "mickey/types/environment_t.h"
#include "mickey/types/integer_t.h"
#include "mickey/types/lambda_t.h"
#include "mickey/types/number_t.h"
#include "mickey/types/pointer_t.h"
#include "mickey/types/port_t.h"
#include "mickey/types/radix_t.h"
#include "mickey/types/rational_t.h"
#include "mickey/types/real_t.h"
#include "mickey/types/symbol_t.h"
#include "mickey/types/syntax_t.h"
#include "mickey/types/type_t.h"
#include "mickey/types/vector_t.h"

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
