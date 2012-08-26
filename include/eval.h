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

#ifndef INC_MICKEY_EVAL_H
#define INC_MICKEY_EVAL_H

#include "cons.h"
#include "parser.h"

/*
 * Magic symbols used to store a procedure's arguments and body.
 *
 * TODO: Do NOT store this directly in the environment, as it could
 *       lead to name clashes.
 */
extern "C" {

extern const char ARGS[];
extern const char BODY[];
extern std::string func_name;

struct body_env_t {
  cons_t *body;
  environment_t *env;
};

cons_t* eval(cons_t* p, environment_t* env);
cons_t* make_closure(cons_t* args, cons_t* body, environment_t* e);
cons_t* call_lambda(cons_t *p, environment_t* e);
body_env_t expand_lambda(cons_t *p, environment_t* e);

}

#endif
