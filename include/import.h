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

#ifndef INC_MICKEY_MODULE_IMPORT_H
#define INC_MICKEY_MODULE_IMPORT_H

#include "cons.h"

struct named_function_t {
  const char* name;

  /*
   * Pointer to native C function or call_lambda
   */
  lambda_t function;

  /*
   * If true, eval will not evaluate this procedure's arguments before
   * entering it.
   */
  bool syntactic;

  /*
   * TODO:
   * - Add number of arguments here for quickly checking for errors
   * - Add rest_args flag here instead of deciding at eval time
   * - Add cons_t* body here and cons_t* args/signature instead of using
   *   __ARGS__ and __BODY__.
   */
};

extern "C" {

extern named_function_t exports_import[];

void import(environment_t*, named_function_t*, const std::string& lib_name = "");
void import_defaults(environment_t*);
void load(const std::string& file, environment_t*);

cons_t* proc_import(cons_t*, environment_t*);
environment_t* import_library(const std::string& name);
bool supports_library(const char* s);
void scan_for_library_files();

}

#endif
