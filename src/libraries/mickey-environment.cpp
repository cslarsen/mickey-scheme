/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2012 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 * ------------------------------------------------------------------------
 *
 * This module presents a first class environment to the
 * user.  It is directly modeled on the API provided
 * by MIT Scheme:
 *
 * http://sicp.ai.mit.edu/Fall-2004/manuals/scheme-7.5.5/doc/scheme_14.html
 *
 * Currently unimplemented:
 *
 * - Immutable values in mickey core
 *
 * - Environment variables system-global-environment and
 *   user-initial-environment
 *
 * - Procedures (nearest-repl/environment) and (ge environment)
 *
 * - Library special forms not really supported in mickey, because
 *   mickey will extend environments when calling them (so, we
 *   need to change mickey itself for this)
 *
 * - procedure (interpreter-environment? object)
 *
 * Also note that the eval procedure is exported as
 * environment-eval.
 *
 * Example usage:
 *
 *   (import (mickey environment))
 *   (define env (make-environment))
 *   (environment-eval (begin ...) env)
 *
 * ------------------------------------------------------------------------
 */

#include "mickey.h"

extern "C" {

cons_t* proc_envp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(type_of(car(p)) == ENVIRONMENT);
}

cons_t* proc_env_has_parentp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(ENVIRONMENT, car(p));
  return boolean(car(p)->environment->outer != NULL);
}

cons_t* proc_env_parent(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(ENVIRONMENT, car(p));

  if ( car(p)->environment->outer == NULL )
    raise(runtime_exception("Environment has no parent"));

  return environment(car(p)->environment->outer);
}

cons_t* proc_env_bound_names(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(ENVIRONMENT, car(p));

  environment_t *e = car(p)->environment;
  cons_t *r = list(), *end = r;

  for ( dict_t::const_iterator i = e->symbols.begin();
        i != e->symbols.end(); ++i )
  {
    // append name only
    end->car = symbol((*i).first.c_str(), NULL);
    end->cdr = cons(NULL);
    end = end->cdr;
  }

  return r;
}

cons_t* proc_env_bindings(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(ENVIRONMENT, car(p));

  environment_t *e = car(p)->environment;
  cons_t *r = list(), *end = r;

  for ( dict_t::const_iterator i = e->symbols.begin();
        i != e->symbols.end(); ++i )
  {
    cons_t *name = symbol((*i).first.c_str(), NULL);
    cons_t *value = (*i).second;

    // if no value, add only name, else (name value)
    end->car = nullp(value)? name : list(name, value);
    end->cdr = cons(NULL);
    end = end->cdr;
  }

  return r;
}

cons_t* proc_env_boundp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(ENVIRONMENT, car(p));
  assert_type(SYMBOL, cadr(p));

  const std::string name = symbol_name(cadr(p));
  environment_t *e = car(p)->environment;

  // follow parent environments
  return boolean(e->lookup(name) != NULL);
}

cons_t* proc_env_lookup(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(ENVIRONMENT, car(p));
  assert_type(SYMBOL, cadr(p));

  const std::string name = symbol_name(cadr(p));
  environment_t *e = car(p)->environment;

  // follow parent environments, don't signal error
  // if not found (TODO: Correct?)
  return nil_coalesce(e->lookup(name));
}

cons_t* proc_env_assignablep(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(ENVIRONMENT, car(p));
  assert_type(SYMBOL, cadr(p));

  const std::string name = symbol_name(cadr(p));
  environment_t *e = car(p)->environment;
  cons_t *val = e->lookup(name);

  if ( val == NULL )
    raise(runtime_exception(
      "Symbol is not bound in any environment: " + name));

  fprintf(stderr,
    "WARNING: Mickey does not support immutable values"
    "yet\n");

  return boolean(true);
}

cons_t* proc_env_assign(cons_t* p, environment_t*)
{
  assert_length(p, 3);
  assert_type(ENVIRONMENT, car(p));
  assert_type(SYMBOL, cadr(p));

  const std::string name = symbol_name(cadr(p));
  environment_t *e = car(p)->environment;
  cons_t *value = caddr(p);

  if ( value == NULL )
    raise(runtime_exception(
      "Symbol is not bound in any environment: " + name));

  environment_t *i = e;

  // search for definition and set if found
  for ( ; i != NULL; i = i->outer ) {
    if ( i->symbols.find(name) != i->symbols.end() ) {
      i->symbols[name] = value;
      return nil();
    }
  }

  // only set if NOT found
  if ( i == NULL )
    e->define(name, value);

  return nil();
}

cons_t* proc_make_environment(cons_t* p, environment_t* e)
{
  /*
   * TODO: Since mickey DOES extend environment even for
   *       library-defined syntactic keywords, we do
   *       it doubly here.  Should fix so that mickey
   *       does not extend environments for special forms.
   */
  e = e->extend();
  eval(cons(symbol("begin", NULL), p), e);
  return environment(e);
}

cons_t* proc_the_environment(cons_t*, environment_t* e)
{
  /*
   * TODO: This should be a _special form_.  See above notes.
   */
  return environment(e);
}

cons_t* proc_boundp(cons_t* p, environment_t* e)
{
  assert_length(p, 1);
  assert_type(SYMBOL, car(p));
  return boolean(e->lookup(car(p)->symbol->name()) != NULL);
}

cons_t* proc_env_eval(cons_t* p, environment_t* e)
{
  assert_length(p, 2);

  // evaluate environment argument
  cons_t *env = eval(cadr(p), e);
  assert_type(ENVIRONMENT, env);

  return eval(car(p), env->environment);
}

named_function_t exports_mickey_environment[] = {
  {"bound?", proc_boundp, false},
  {"environment-assign!", proc_env_assign, false},
  {"environment-assignable?", proc_env_assignablep, false},
  {"environment-bindings", proc_env_bindings, false},
  {"environment-bound-names", proc_env_bound_names, false},
  {"environment-bound?", proc_env_boundp, false},
  {"environment-eval", proc_env_eval, true},
  {"environment-has-parent?", proc_env_has_parentp, false},
  {"environment-lookup", proc_env_lookup, false},
  {"environment-parent", proc_env_parent, false},
  {"environment?", proc_envp, false},
  {"make-environment", proc_make_environment, true},
  {"the-environment", proc_the_environment, false},
  {NULL, NULL, false}
};

}
