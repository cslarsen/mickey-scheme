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

#ifndef INC_MICKEY_ENVIRONMENT_H
#define INC_MICKEY_ENVIRONMENT_H

#include <string>
#include "dict_t.h"
#include "lambda_t.h"

/*
 * Environment holds a symbol table and points to an optional outer (or
 * parent) environment.
 */
struct environment_t {
  struct environment_t *outer;
  dict_t symbols;

  environment_t* extend();
  struct cons_t* lookup(const std::string& name) const;
  struct cons_t* lookup_or_throw(const std::string& name) const;
  struct cons_t* define(const std::string& name, lambda_t func, bool syntactic = false);
  struct cons_t* define(const std::string& name, cons_t* body);
  environment_t* outermost();

private:
  environment_t() : outer(NULL)
  {
  }

  /*
   * Restrict instantiation to these functions:
   */
  friend environment_t* null_environment(int);
  friend environment_t* null_import_environment();

  /*
   * Prevent copy construction and assignment:
   */
  environment_t(const environment_t&);
  environment_t& operator=(const environment_t&);
};

/*
 * Merges the two environments by copying `b´ into `a´.
 * Returns number of symbols copied.
 */
int merge(environment_t *to, const environment_t *from);

#endif
