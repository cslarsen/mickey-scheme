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

struct cont_t {
  cons_t* proc; // or atom

  std::vector<cons_t*> args;
  size_t index; // current argument

  cons_t* result; // return value
  cont_t* next; // continuation waiting for this result

  cont_t(cons_t* procedure,
         cons_t* arguments,
         cont_t* continuation):
    proc(procedure),
    index(0),
    result(NULL),
    next(continuation)
  {
    for ( cons_t *p = arguments; !nullp(p); p = cdr(p) )
      args.push_back(car(p));
  }

  /*
   * Sets return value for this continuation and passes it to the
   * continuation.  Returns the next continuation.
   */
  cont_t* set_return_value(cons_t* value)
  {
    if ( next != NULL )
      result = next->args[next->index++] = value;

    return next;
  }
};

static bool selfevalp(cons_t* p)
{
  return booleanp(p)
      || charp(p)
      || closurep(p)
      || emptylistp(p)
      || environmentp(p)
      || numberp(p)
      || portp(p)
      || stringp(p)
      || syntaxp(p)
      || vectorp(p);
}

static bool foreignp(closure_t* p)
{
  return p->function != NULL;
}

/*
 * Call foreign procedure.
 */
static cons_t* call_foreign(const char* proc_name,
                            closure_t* proc,
                            cons_t* args,
                            cont_t*, environment_t*)
{
  lambda_t func = proc->function;

  if ( func == NULL )
    raise(runtime_exception(format(
      "Foreign procedure %s has no function pointer", proc_name)));

  return func(args, proc->environment);
}

/*
 * Bind parameters and return continuation for calling a source code
 * procedure.
 */
static cont_t* call_source(const char* proc_name,
                           closure_t* proc,
                           cons_t* args,
                           cont_t* cc,
                           environment_t* e)
{
  printf("apply %s ", to_s(proc).c_str());
  printf("with %s\n", sprint(args).c_str());

  size_t received_args = length(args);
  size_t required_args = length(proc->args);
  bool variadic = variadicp(proc->args);

  printf("proc %s req %ld args, variadic=%d\n",
      proc_name, required_args, variadic);

  if ( received_args < required_args )
    raise(runtime_exception(format(
      "Procedure %s requires %d parameters but got %d",
      proc_name, required_args, received_args)));

  if ( received_args > required_args && !variadic ) {
    if ( required_args == 0 )
      raise(runtime_exception(format(
        "Procedure %s accepts no arguments but was given %d: %s",
        proc_name, received_args, sprint(args).c_str())));
    else
      raise(runtime_exception(format(
        "Procedure %s only accepts %d parameters but got %d: %s",
        proc_name, required_args, received_args, sprint(args).c_str())));
  }

  // extend environment and bind parameter values to names
  e = e->extend();

  for ( cons_t *value = args, *name = proc->args;
        !nullp(name);
        value = cdr(value), name = cdr(name) )
  {
    /*
     * Detect pure variadic function, e.g. (lambda x (display x)) will have
     * all args placed in x.
     */
    if ( !symbolp(car(name)) ) {
      if ( !variadic )
        raise(runtime_exception("Lambda argument not a symbol but type "
          + to_s(type_of(car(name))) + ": " + sprint(car(name))));

      // If value is nil, give it a default value
      e->define(*name->symbol, !nullp(value)? value : list(NULL));
      break;
    }

    /*
     * Variadic function with a minimal number of arguments, i.e. a function
     * on the form (lambda (x y z . rest) ...)
     */
    if ( !pairp(name) ) {
      /*
       * Function called with minimum number of required values, i.e. the
       * lambda above got called with three arguments.  Rest will therefore
       * be given a default value.
       */
      if ( nullp(value) )
        value = list(NULL);

      e->define(*cadr(name)->symbol, value);
      break;
    }

    if ( nullp(value) )
      break;

    e->define(*car(name)->symbol, car(value));
  } // for name/value

  /*
   * Now let's execute body with given definition, i.e. we have transformed
   *
   * (define (foo x) <body>)
   *
   * to:
   *
   * (set! name1 value1)
   * (set! name2 value2)
   * (set! nameN valueN)
   * (<body>)
   *
   * within a new, extended environment.
   *
   * We won't call eval here, we will simply return a continuation that can
   * be executed.
   */
  cons_t *cl = closure(NULL, e, false);
  return new cont_t(cl, nil(), cc);
}

cons_t* list(const std::vector<cons_t*>& v)
{
  cons_t *r = list();

  for ( int n=v.size()-1; n>=0; --n )
    r = cons(v[n], r);

  return r;
}

extern "C" cons_t* proc_eval_cont(cons_t* p, environment_t* e)
{
  printf("program: %s\n", sprint(p).c_str());

  // Set up a final continuation
  cont_t *cc = new cont_t(symbol("display"), p, NULL);

  do {
    printf("%p cc proc=%s", cc, sprint(cc->proc).c_str());
    printf(" args=%s", sprint(list(cc->args)).c_str());
    printf(" index=%lu length=%lu next=%p\n", cc->index, cc->args.size(), cc->next);

    // evaluate remaining arguments
    if ( cc->index < cc->args.size() ) {
      printf("evlis %s ", sprint(cc->proc).c_str());

      cons_t *expr = cc->args[cc->index];
      cons_t *proc = car(expr);
      cons_t *args = cdr(expr);

      if ( !atomp(expr) )
        cc = new cont_t(proc, args, cc);
      else
        cc = new cont_t(expr, nil(), cc);

      printf("argument %s\n", sprint(expr).c_str());
      continue;
    }

    cons_t *proc = cc->proc;
    cons_t *args = list(cc->args);

    // all arguments have been evaluated, now apply function
    if ( atomp(proc) ) {
      if ( symbolp(proc) ) {
        cons_t *lookup = e->lookup_or_throw(*proc->symbol);

        if ( closurep(lookup) ) {
          if ( foreignp(lookup->closure) ) {
            cons_t *res = call_foreign(proc->symbol->c_str(), lookup->closure, args, cc, e);
            cc = cc->set_return_value(res);
            printf("foreign result = %s -- cc=%p\n", sprint(res).c_str(), cc);
            if ( cc == NULL ) return res; // end eval?
            continue;
          }

          cc = call_source(proc->symbol->c_str(), lookup->closure, args, cc, e);
          printf("calling source code");
          continue;
        }

        // looked up form
        cc = cc->set_return_value(lookup);
        printf("lookup result = %s\n", sprint(lookup).c_str());
        if ( cc == NULL ) return lookup; // end eval?
        continue;
      }

      // self-evaluating forms such as numbers, strings, etc.
      if ( selfevalp(proc) && nullp(args) ) {
        cc = cc->set_return_value(proc);
        printf("selfeval result = %s\n", sprint(proc).c_str());
        if ( cc == NULL ) return proc; // end eval?
        continue;
      }

      printf("unknown thing: %s\n", sprint(proc).c_str());
    }
  } while ( cc != NULL );

  return unspecified(nil());
}
