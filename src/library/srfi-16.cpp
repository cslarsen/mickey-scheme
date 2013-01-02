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

#include "mickey.h"

extern "C" {

/*
 * Transform
 *
 * (case-lambda
 *   ((<form1> <body1>)
 *    (<form2> <body2>)
 *    ...)
 *
 * to
 *
 * (lambda args
 *   (cond
 *     (((if (variadic? <form1>) >= =) (length args) <form1-min-args>)
 *      (apply (lambda (<form1>) <body1>) args))
 *     ...))
 */
cons_t* proc_case_lambda(cons_t* p, environment_t* e)
{
  cons_t *cond_cases = list();

  cons_t *cases = p;

  for ( cons_t* c = cases; !nullp(c); c = cdr(c) ) {
    cons_t *formals = caar(c);
    cons_t *body = cdar(c);

    // ((if (variadic? <form1>) >= =) argc <form1-min-args>)
    cons_t* cond_if =
      cons(symbol(variadicp(formals)? ">=" : "="),
      cons(cons(symbol("length"), cons(symbol("args"))),
      cons(integer(min_args(formals)))));

    // (apply (lambda (<form1>) <body1>) args)
    cons_t *cond_then =
      cons(symbol("apply"),
      cons(cons(symbol("lambda"),
           cons(formals, body)),
      cons(symbol("args"))));

    cond_cases = append(cond_cases, list(list(cond_if, cond_then)));
  }

  cond_cases = splice(cons(symbol("cond")), cond_cases);

  return make_closure(symbol("args"), cons(cond_cases), e);
}

} // extern "C"
