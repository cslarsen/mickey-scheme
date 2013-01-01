/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2013 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include <stdlib.h>
#include <dlfcn.h>
#include <cassert>
#include "cons.h"
#include "primitives.h"
#include "import.h"
#include "assertions.h"
#include "exceptions.h"
#include "print.h"
#include "options.h"
#include "eval.h"
#include "file-io.h"
#include "debug.h"
#include "system-features.h"
#include "import.h"

/*
 * Cond-expand is Scheme's conditional code expansion system, akin to #ifdef
 * for C.
 *
 * Although now a part of R7RS it originally came from SRFI-0:
 * http://srfi.schemers.org/srfi-0/srfi-0.html
 *
 * It has the following form:
 *
 * (cond-expand <clause> ... (else <body>))
 *
 * where <clause> can be
 *   - (<feature req> <body> ...)
 *
 * where <feature req> can be any combination of:
 *
 *   - <feature>
 *   - (library <name>)
 *   - (and <feature> ...)
 *   - (or <feature> ...)
 *   - (not <feature>)
 *
 * We will copy the clauses and transform each <feature> and (library
 * <name>) into #t if supported and #f if not.  We will then evaluate the
 * resulting expression as a normal Scheme boolean expression.
 *
 * If the <clause> returns #t then the <body> will be included in the
 * library.
 */

extern "C" cons_t* cond_expand(const cons_t* p, environment_t*);

/*
 * Returns TRUE or FALSE on whether given feature requirement is supported
 * or not.
 *
 * A <feature requirement> takes one of the following forms:
 *
 *   - <feature identifier>
 *   - (library <library name>)
 *   - (and <feature requirement> ...)
 *   - (or <feature requirement> ...)
 *   - (not <feature requirement>)
 */
static cons_t* lookup_feature_requirement(const cons_t* p)
{
  // <feature identifier>
  if ( symbolp(p) )
    return boolean(supports_feature(p->symbol->c_str()));

  if ( pairp(p) ) {
    std::string op = symbol_name(car(p));

    // (library <library name>)
    if ( op == "library" ) {
      if ( symbolp(cadr(p)) )
        return boolean(supports_library(cadr(p)->symbol->c_str()));

      raise(runtime_exception("Missing <name> in (library <name>)"));
    }

    // (and/or/not <feature req> ...)
    if ( op == "and" || op == "or" || op == "not" ) {

      if ( op == "not" && length(p) != 2 )
        raise(runtime_exception(
          "cond-expand (not <feature req>) only takes one argument"));

      cons_t *r = list(symbol(op.c_str()), cons(NULL)), *e = cdr(r);

      for ( cons_t *q = cdr(p); !nullp(q); q = cdr(q) ) {
        e->car = lookup_feature_requirement(car(q));
        e->cdr = cons(NULL);
        e = e->cdr;
      }

      return r;
    }
  }

  raise(runtime_exception(format(
    "Unknown cond-expand feature requirement: %s", sprint(p).c_str())));
  return nil();
}

static bool eval_feature_req(cons_t* p)
{
  if ( booleanp(p) )
    return p->boolean;

  if ( !pairp(p) || !symbolp(car(p)) )
    raise(runtime_exception(format(
      "Invalid feature request: %s", sprint(p).c_str())));

  std::string op = symbol_name(car(p));
  bool r = eval_feature_req(cadr(p));

  if ( op == "else" )
    return true;
  else if ( op == "and" ) {
    for ( p = cddr(p); !nullp(p); p = cdr(p) )
      r &= eval_feature_req(car(p));
  } else if ( op == "or" ) {
    for ( p = cddr(p); !nullp(p); p = cdr(p) )
      r |= eval_feature_req(car(p));
  } else if ( op == "not" )
    r = !r;

  return r;
}

/*
 * Evaluate each cond-expand clause and pass on the bodies of all those that
 * are matched.
 */
extern "C" cons_t* cond_expand(const cons_t* p, environment_t*)
{
  cons_t *r = nil();

  for ( ; !nullp(p); p = cdr(p) ) {
    // clause := (<feature request> <library declaration>)
    cons_t *clause = car(p),
           *req    = car(clause),
           *body   = cadr(clause);

    // if <feature request> evaluates to #t, add the body
    if ( eval_feature_req(lookup_feature_requirement(req)) )
      r = append(r, body);
  }

  return list(symbol("begin"), r);
}
