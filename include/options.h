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

#ifndef INC_MICKEY_OPTIONS_H
#define INC_MICKEY_OPTIONS_H

#include "cons.h"

struct options_t
{
  bool verbose;
  bool read_stdin;
  bool eval_next;
  port_t current_output_device;
  port_t current_input_device;
  port_t current_error_device;
  const char* current_filename;
  const char* include_path;
  const char* lib_path;
  const char* startup_path;
  const char* mickey_absolute_path;
  char **argv;
  int argc;
  bool warn;
  bool empty_repl_env;
};

extern options_t global_opts;

void set_default(struct options_t*, int argc, char** argv);
void set_lib_path(struct options_t*, const char* lib_path);
void reset_for_programs(struct options_t*, const char* file = NULL);

#endif
