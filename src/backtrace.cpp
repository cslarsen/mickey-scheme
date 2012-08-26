/*
 * Mickey Scheme
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *                                                          
 */

#include <stack>
#include "cons.h"
#include "print.h"

// instruction stack for backtraces
static std::stack<cons_t*> is;
static bool trace_stack = true;

void set_backtrace(bool on_off)
{
  trace_stack = on_off;
}

void backtrace_push(cons_t* p)
{
  if ( trace_stack )
    is.push(p);
}

void backtrace_pop()
{
  if ( trace_stack )
    is.pop();
}

void backtrace_clear()
{
  while ( !is.empty() )
    is.pop();
}

/*
 * Print backtrace
 */
void backtrace()
{
  if ( !trace_stack || is.empty() )
    return;

  std::stack<cons_t*> p = is;
  printf("Backtrace:\n");

  while ( !p.empty() ) {
    printf(" - %s\n", sprint(p.top()).c_str());
    p.pop();
  }
}

bool backtracing()
{
  return trace_stack;
}

cons_t* backtrace_top()
{
  return is.empty()? nil() : is.top();
}
