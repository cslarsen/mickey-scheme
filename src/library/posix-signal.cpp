/*
 * UNIX signal.h for Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under the LGPL 2.1; see LICENSE
 */

#include <signal.h>
#include <errno.h>
#include "mickey/mickey.h"

static const int SIGHANDLERS = 31;
static closure_t* sighandlers[SIGHANDLERS] = {NULL};

/*
 * Dispatch signal to closure
 */
static void sighandler(int sig)
{
  if ( sig<0 || sig > SIGHANDLERS )
    return;

  closure_t *proc = sighandlers[sig];

  if ( proc == NULL )
    return;

  cons_t *p = new cons_t();
  p->type = CLOSURE;
  p->closure = proc;

  eval(cons(p, cons(integer(sig))), proc->environment);
}


/*
 * Setup a signal handler closure
 */
static sig_t set_handler(int sig, closure_t* hdl)
{
  sighandlers[sig] = hdl;
  return signal(sig, hdl!=NULL? sighandler : NULL);
}

extern "C" cons_t* proc_signal(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(INTEGER, car(p));
  assert_type(CLOSURE, cadr(p));

  integer_t sig = car(p)->number.integer;
  closure_t* func = cadr(p)->closure;

  if ( sig<0 || sig > SIGHANDLERS )
    raise(runtime_exception(format("Invalid signal: %d", sig)));

  if ( SIG_ERR == set_handler(sig, func) )
    raise(runtime_exception(format("signal: %s", strerror(errno))));

  return nil();
}

extern "C" cons_t* proc_deactivate_signal(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));

  integer_t sig = car(p)->number.integer;

  if ( sig<0 || sig > SIGHANDLERS )
    raise(runtime_exception(format("Invalid signal: %d", sig)));

  if ( SIG_ERR == set_handler(sig, NULL) )
    raise(runtime_exception(strerror(errno)));

  return nil();
}
