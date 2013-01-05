/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2013 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 * This file contains procedures that will eventually be in
 * library/scheme-base.cpp but who cannot be loaded dynamically yet.
 */

#include "mickey.h"
#include "core-transition.h"

/*
 * This file is a placeholder for procedures that are actually syntactic,
 * but must be here because of the broken way we deal with macros and
 * environments / phasing.
 *
 * When that is fixed, the procedures here will be moved back into
 * scheme-base.cpp, where they belong.
 *
 * Ultimately, most of these will be implemented as pure scheme macros.
 */

extern "C" {

cons_t* proc_begin(cons_t* p, environment_t*)
{
  return cons(symbol("begin"), p);
}

/*
 * Utility function used internally in exported functions.
 */
static cons_t* let(cons_t *bindings, cons_t *body)
{
  return cons(symbol("let"), cons(bindings, cons(body)));
}

/*
 * See comments in proc_set_cdr.
 */
cons_t* proc_set_car(cons_t* p, environment_t* e)
{
  cons_t *target = car(p);
  cons_t *source = cadr(p);

  if ( type_of(target) == SYMBOL )
    target = e->lookup_or_throw(*target->symbol);
  else
    target = eval(target, e);

  if ( type_of(target) != PAIR )
    assert_type(PAIR, target); // raise error

  if ( type_of(source) == SYMBOL )
    source = e->lookup_or_throw(*source->symbol);
  else // constant, or whatever
    source = eval(source, e);

  if ( !pairp(target) )
    raise(runtime_exception(format(
      "Not a pair: %s", sprint(target).c_str())));

  target->car = source;
  return unspecified();
}

/*
 * set-cdr! is intercepted by eval, so we can look up
 * addresses ourselves.
 *
 * We do it this way so we can create circular cells.
 */
cons_t* proc_set_cdr(cons_t* p, environment_t* e)
{
  cons_t *target = car(p);
  cons_t *source = cadr(p);

  if ( type_of(target) == SYMBOL )
    target = e->lookup_or_throw(*target->symbol);
  else
    // need to support the situation:
    // (define a (list 1 2 3)) (set-cdr! (cdr a) 99) a
    target = eval(target, e); // e.g. "(cdr foo)"

  if ( type_of(target) != PAIR )
    assert_type(PAIR, target); // raise error

  if ( type_of(source) == SYMBOL )
    source = e->lookup_or_throw(*source->symbol);
  else // constant, or whatever
    source = eval(source, e);

  if ( !pairp(target) )
    raise(runtime_exception(format(
      "Not a pair: %s", sprint(target).c_str())));

  target->cdr = source;
  return unspecified();
}

/*
 * Transform:
 *
 * (cond ((case-1) action-1 ...)
 *       ((case-2) action-2 ...)
 *       ((case-n) action-n ...)
 *       (else <else action>))
 *
 * to
 *
 *  (let ((result <case-1>))
 *    (if result (begin action-1 ...)
 *      (let ((result <case-2>))
 *        (if result (begin <action-2> ...)
 *          (let ((result <case-n>))
 *            (if result (begin <action-n>))
 *              <else action>)))))
 *
 * Also, forms involving the literal `=>´ will be
 * transformed like the following:
 *
 *  ((<case> => <action>))
 *
 * is transformed to
 *
 * (begin (<action> result))
 *
 */
cons_t* proc_cond(cons_t* p, environment_t* e)
{
  cons_t *r = list(NULL);
  p = cdr(p);

  if ( nullp(p) )
    return r;

  cons_t   *test = caar(p),
         *action = car(cdar(p));

  if ( symbol_name(action) == "=>" )
    action = cons(cadr(cdar(p)), cons(symbol("result")));

  cons_t *otherwise = proc_cond(p, e);

  return (symbol_name(test) == "else")? action :
    let(list(list(symbol("result"), test)), // (let ((result <test>))
      cons(symbol("if"),                    //   (if result
        cons(symbol("result"),              //     (begin <action>)
          cons(cons(symbol("begin"),        //       <otherwise>))
            cons(action)),
              cons(otherwise)))));
}

/*
 * Transforms
 *
 *  (case <key>
 *    ((<datum> ...) <expression> ...)
 *    ((<datum> ...) => <expression>)
 *    (else <expression> ...) ; OR
 *    (else => <expression>))
 *
 * to the code
 *
 *  (let
 *    ((value <key>))
 *    (cond
 *      ((memv value (quote (<datum> ...))) <expression> ...)
 *      ((memv value (quote (<datum> ...))) => <expression>)
 *      (else <expression>) ; OR
 *      (else => <expression>)))
 *
 */
cons_t* proc_case(cons_t *p, environment_t*)
{
  cons_t *key = cadr(p);
  cons_t *clauses = cons(NULL);

  for ( cons_t *c = cddr(p); !nullp(c); c = cdr(c) ) {
    cons_t *datum = caar(c);
    cons_t *exprs = cdar(c);

    if ( symbol_name(datum) == "else" ) {
      if ( symbol_name(cadar(c)) == "=>" ) {
        // produces: (else (<expression / procedure> <key>))
        clauses = append(clauses,
                      cons(cons(symbol("else"),
                        cons(cons(cadr(cdar(c)),
                          cons(symbol("value")))))));
      } else
        clauses = append(clauses, c);

      break;
    }

    if ( symbol_name(cadar(c)) == "=>" )
      // produces: (<expression / procedure> <key>)
      exprs = list(cons(car(cdr(cdar(c))), cons(symbol("value"))));

    cons_t *clause =
      cons(symbol("memv"),
        cons(symbol("value"),
          cons(cons(symbol("quote"), cons(datum)))));

    clause = splice(cons(clause), exprs);
    clauses = append(clauses, cons(clause));
  }

  cons_t *let = append(
    cons(symbol("let"),
      cons(cons(cons(symbol("value"), cons(key))))),
    cons(cons(symbol("cond"), clauses)));

  return let;
}

cons_t* proc_define(cons_t *p, environment_t *env)
{
  // (define <name> <body>)
  assert_type(SYMBOL, car(p));
  cons_t *name = car(p);
  cons_t *body = cadr(p);

  if ( name->symbol == NULL || name->symbol->empty() )
    raise(runtime_exception("Cannot define with empty variable name")); // TODO: Even possible?

  env->define(*name->symbol, body);
  return unspecified();
}

cons_t* proc_define_syntax(cons_t *p, environment_t *env)
{
  // (define-syntax <name> <body>)
  assert_type(SYMBOL, car(p));
  cons_t *name = car(p);
  cons_t *syntax = cadr(p);

  if ( name->symbol == NULL || name->symbol->empty() )
    raise(runtime_exception("Cannot define-syntax with empty name"));

  env->define(*name->symbol, syntax);
  return unspecified();
}

/*
 * Expand according to r7rs, pp. 56:
 *
 *  (letrec
 *    ((loop (lambda (<var1> <var2> <varN>) 
 *         (if <test>
 *             (begin
 *               (if #f #f) ; used to get unspecified value
 *               <expr>)
 *             (begin
 *               <command>
 *               (loop (<step1> <step2> <stepN>)))))))
 *    (loop <init1> <init2> <initN>))
 */
cons_t* proc_do(cons_t* p, environment_t*)
{
  cons_t *vars = cadr(p),
         *test = caaddr(p),
         *test_body = cons(symbol("begin"), cdaddr(p)),
         *body = cons(symbol("begin"), cdddr(p));

  // find variable names, initial values and stgeps
  cons_t *names = list(NULL),
         *init  = list(NULL),
         *step  = list(NULL);

  for ( cons_t *n = vars; !nullp(n); n = cdr(n) ) {
    names = append(names, cons(caar(n)));
    init  = append(init, cons(cadar(n)));
    step  = append(step, cons(caddar(n)));
  }

  // (loop <step1> <step2> <stepN>)
  cons_t* loop = cons(symbol("loop"));
  for ( cons_t *n = step; !nullp(n); n = cdr(n) )
    loop = append(loop, cons(car(n)));

  // (begin <body> (loop ...))
  body = append(body, cons(loop));

  // (if <test> <test_body> (begin <body> (loop ...))
  cons_t *if_expr =
    cons(symbol("if"),
      cons(test,
        cons(test_body,
          cons(body))));

  // (lambda (<var1> <var2> <varN>) <if_expr>)
  cons_t *lambda =
    cons(symbol("lambda"),
      cons(names,
        cons(if_expr)));

  // (loop <lambda>)
  loop =
    cons(symbol("loop"),
      cons(lambda));

  // (loop <init1> <init2> <initN>)
  cons_t *loop_init =
    cons(symbol("loop"),
      init);

  // (letrec ((loop <loop_body)) (loop <init1> ...))
  cons_t *letrec =
    cons(symbol("letrec"),
      cons(list(loop),
       cons(loop_init)));

  return letrec;
}

/*
 * Transform to lambdas:
 *
 * (let <name>
 *      ((name-1 value-1)
 *       (name-2 value-2)
 *       (name-n value-n))
 *       <body>)
 *
 * to
 *
 * ((lambda (name-1 name-2 name-n)
 *    <body>) value-1 value-2 value-n)
 *
 * If an OPTIONAL <name> is given, we have a "named let"
 * which will produce the output:
 *
 * (letrec
 *   ((<name> (lambda
 *              (name-1 name-2 name-n)
 *              <body>)))
 *   (<name> value-1 value-2 value-n))
 *
 */
cons_t* proc_let(cons_t* p, environment_t* e)
{
  cons_t *name = NULL;

  // named let?
  if ( symbolp(car(p)) ) {
    name = car(p);
       p = cdr(p);
  }

  cons_t *body = cdr(p),
        *names = list(NULL),
       *values = list(NULL);

  for ( cons_t *n = car(p); !nullp(n); n = cdr(n) ) {
     names = append(names, list(caar(n)));
    values = append(values, list(car(cdar(n))));
  }

  /*
   * Normal let:
   *
   * Build lambda expression and return it, eval will eval it :)
   * (or we could call make_closure here):
   *
   * ((lambda (<names>) <body>) <values>)
   *
   */
  if ( !name )
    return cons(cons(symbol("lambda"),
           cons(names, cons(proc_begin(body, e)))), values);

  /*
   * Transform named let to letrec.
   */
  return cons(symbol("letrec"),               // (letrec
           cons(cons(cons(name,               //   ((<name>
             cons(cons(symbol("lambda"),      //      (lambda
               cons(names,                    //        (name-1 .. name-n)
               cons(proc_begin(body, e))))))),//        (begin <body>))))
           cons(cons(name, values))));        //   (<name> value-1 .. value-n))
}

/*
 * Transform to begin-define with dummy initial values
 * and using set! to update with correct ones:
 *
 * (letrec ((name-1 value-1)
 *          (name-2 value-2)
 *          (name-3 value-3))
 *         <body>)
 * to
 *
 * (let ((name-1 #f)
 *       (name-2 #f)
 *       (name-3 #f))
 *   (set! name-1 value1)
 *   (set! name-2 value2)
 *   (set! name-3 value3)
 *    <body>)
 */
cons_t* proc_letrec(cons_t* p, environment_t*)
{
  cons_t  *body  = cdr(p),
          *names = list(NULL),
         *values = list(NULL);

  for ( cons_t *n = car(p); !nullp(n); n = cdr(n) ) {
     names = cons(caar(n), names);
    values = cons(cadar(n), values);
  }

  // do not clutter other environments
  cons_t *r = NULL;

  // (define name-N #f)
  for ( cons_t *n=names; !nullp(n); n=cdr(n) )
    r = append(r, cons(cons(car(n),
                       cons(boolean(false)))));

  // wrap in: (let ((key value) ...))
  r = cons(symbol("let"), list(r));

  // (set! name-N value-N)
  for ( cons_t *n=names,
               *v=values;
        !nullp(n) && !nullp(v);
        n=cdr(n), v=cdr(v) )
  {
    r = append(r, cons(cons(symbol("set!"),
                              cons(car(n),
                              cons(car(v))))));
  }

  // add <body>
  r = append(r, body);

  return r;
}

/*
 * Transform to nested lambdas:
 *
 * (let* ((name-1 value-1)
 *        (name-2 value-2)
 *        (name-3 value-3))
 *        <body>)
 * to
 *
 * ((lambda (name-1)
 * ((lambda (name-2)
 * ((lambda (name-3)
 *   <body>) value-3))
 *           value-2))
 *           value-1)
 */
cons_t* proc_letstar(cons_t* p, environment_t* e)
{
  cons_t  *body  = cdr(p),
          *names = list(NULL),
         *values = list(NULL);

  for ( cons_t *n = car(p); !nullp(n); n = cdr(n) ) {
     names = cons(caar(n), names);
    values = cons(cadar(n), values);
  }

  // Work our way outward by constructing lambdas
  cons_t *inner = proc_begin(body, e);

  while ( !nullp(names) && !nullp(values) ) {
    inner = cons(cons(symbol("lambda"),
      cons(cons(car(names)), cons(inner))), cons(car(values)));

     names = cdr(names);
    values = cdr(values);
  }

  return inner;
}

cons_t* proc_vector(cons_t* p, environment_t*)
{
  return vector(p);
}

/*
 * Evaluate from left to right, and return first expression that returns
 * true, disregarding the rest.
 *
 * Note that (or) is a syntactic form; it ONLY evaluates the parameters it
 * needs.  Therefore a good test to see if (or) has been correctly
 * implemented is to insert an invalid statement and check that it skips it.
 * Thus, this should return 1:
 *
 * (or 1 (/ 123 0))
 * (or 1 (/ *))
 *
 */
cons_t* proc_or(cons_t* p, environment_t* e)
{
  /*
   * (or) should return #t.
   */
  for(;;) {
    if ( nullp(p) )
      break;

    // true? then stop and return it
    cons_t *element = eval(car(p), e);

    if ( !boolean_false(element) )
      return element;

    p = cdr(p);
  }

  return boolean(false);
}

/*
 * If any expression evaluates to #f,
 * stop and return #f.  If not, return
 * value of last expression.
 */
cons_t* proc_and(cons_t* p, environment_t* e)
{
  /*
   * (and) should return #t.
   */
  cons_t* last = boolean(true);

  for(;;) {
    if ( nullp(p) )
      return last;

    // false? then stop evaluating
    cons_t *element = eval(car(p), e);
    last = element;

    if ( boolean_false(element) )
      return boolean(false);

    p = cdr(p);
  }

  return last;
}

} // extern "C"
