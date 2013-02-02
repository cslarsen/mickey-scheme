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

#include <stdio.h>
#include <stdlib.h>
#include <stack>
#include "exceptions.h"

std::stack<jmp_buf*> catch_points;
general_exception __exception;

jmp_buf* catch_point()
{
  if ( catch_points.empty() ) {
    fprintf(stderr, "Fatal internal error: Exception stack underflow\n");
    exit(1);
  }

  return catch_points.top();
}

void push_catch_point()
{
  catch_points.push(static_cast<jmp_buf*>(malloc(sizeof(jmp_buf))));
}

void pop_catch_point()
{
  catch_points.pop();
}

void raise(const exception_t& e)
{
  __exception = general_exception(e.what());

  // Uncaught exception?
  if ( catch_points.empty() ) {
    fprintf(stderr, "Uncaught exception: %s\n", e.what());
    fprintf(stderr, "Sorry for calling abort() here and crash-reporting, but\n"
                    "if you don't like that, you can just change the code to\n"
                    "whatever you want.\n");
    abort();
  }

  jmp_buf *jmp = catch_point();
  pop_catch_point();
  longjmp(*jmp, 1);
}
