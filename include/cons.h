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

#include "type/bytevector_t.h"
#include "type/character_t.h"
#include "type/closure_t.h"
#include "type/continuation_t.h"
#include "type/decimal_t.h"
#include "type/dict_t.h"
#include "type/environment_t.h"
#include "type/lambda_t.h"
#include "type/pointer_t.h"
#include "type/port_t.h"
#include "type/symbol_t.h"
#include "type/syntax_t.h"
#include "type/type_t.h"
#include "type/vector_t.h"

/*
 * A variant variable.  Called a cons-cell, but that's incorrect.
 *
 * TODO: To cons_t, Add `marked` (for GC) and `mutable/immutable` (per spec)
 */
struct cons_t {
  type_t type;
  bool exact; // TODO: Move into own number type, or something
  union {
    bool boolean;
    character_t character;
    int integer;
    decimal_t decimal;
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
