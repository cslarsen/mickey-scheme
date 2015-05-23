/*
 * Mickey Scheme
 *
 * Copyright (C) 2011, 2015 Christian Stigen Larsen
 * Distributed under the LGPL 2.1; see LICENSE
 *
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <stdlib.h>
#include <unistd.h>
#include <sys/param.h>
#include <libgen.h>

#include "mickey/mickey.h"
#include "mickey/options.h"
#include "mickey/cons.h"
#include "mickey/parser.h"
#include "mickey/system-features.h"

options_t global_opts;

void set_default(struct options_t* p, int argc, char** argv)
{
  p->verbose = false;
  p->read_stdin = false;
  p->include_path = "";
  p->argc = argc;
  p->argv = argv;
  p->warn = false;
  p->gc = false;
  p->gc_iterations = 1000;
  p->gc_verbose = false;

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

  // TODO: This is really dirty and should be fixed. We want a better way to
  // find the library files.  Perhaps the shared objects should be placed in
  // the share-folder instead, along with the Scheme library files.
  char *lp = realpath(format("%s/../share/mickey/" PACKAGE_VERSION "/lib/",
                             p->mickey_absolute_path).c_str(), NULL);
  if ( lp != NULL ) {
    p->mickey_absolute_lib_path = strdup(lp);
    free(lp);
  } else if ( p->mickey_absolute_lib_path == NULL ) {
    p->mickey_absolute_lib_path = strdup(p->mickey_absolute_path);
  }

  /*
   * Set up feature environment
   */
  detect_features();

  reset_for_programs(p);
}

void add_lib_path(struct options_t* p, const char* lib_path)
{
  p->lib_path.push_back(strdup(lib_path));
}

void reset_for_programs(struct options_t* p, const char* file)
{
  p->current_output_device = port_t(stdout).output().textual();
  p->current_input_device = port_t(stdin).input().textual();
  p->current_error_device = port_t(stderr).output().textual();
  p->current_filename = file;
}
