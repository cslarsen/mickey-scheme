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

#include <unistd.h>
#include <sys/param.h>
#include <libgen.h>
#include "mickey.h"
#include "options.h"
#include "cons.h"
#include "parser.h"

options_t global_opts;

void set_default(struct options_t* p, int argc, char** argv)
{
  p->verbose = false;
  p->read_stdin = false;
  p->include_path = "";
  p->lib_path = "";
  p->argc = argc;
  p->argv = argv;
  p->warn = false;

  /*
   * Current working directory at time of execution.
   */
  static char s[1+MAXPATHLEN];
  p->startup_path = getcwd(s, MAXPATHLEN);

  /*
   * Absolute path to directory that contains mickey executable.
   */
  static char t[1+MAXPATHLEN];
  p->mickey_absolute_path = dirname(realpath(argv[0], t));

  reset_for_programs(p);
}

void set_lib_path(struct options_t* p, const char* lib_path)
{
  p->lib_path = strdup(lib_path);
}

void reset_for_programs(struct options_t* p, const char* file)
{
  p->current_output_device = port_t(stdout).output().textual();
  p->current_input_device = port_t(stdin).input().textual();
  p->current_error_device = port_t(stderr).output().textual();
  p->current_filename = file;
}
