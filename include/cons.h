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

#include "bytevector_t.h"
#include "closure_t.h"
#include "continuation_t.h"
#include "decimal_t.h"
#include "dict_t.h"
#include "environment_t.h"
#include "lambda_t.h"
#include "pointer_t.h"
#include "port_t.h"
#include "symbol_t.h"
#include "syntax_t.h"
#include "type_t.h"
#include "vector_t.h"

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
    char character;
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
