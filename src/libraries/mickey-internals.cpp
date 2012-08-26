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

#include "mickey.h"

extern "C" {

cons_t* global_options(cons_t* p, environment_t*)
{
  assert_length(p, 0);

  /*
   * Only spit out useful options.  E.g., don't return argc/argv, because
   * (scheme command-line) can do that for you.
   */
  return
    cons(list(symbol("be-verbose", NULL),
              boolean(global_opts.verbose)),

    cons(list(symbol("current-output-device", NULL),
              port(&global_opts.current_output_device)),

    cons(list(symbol("current-input-device", NULL),
              port(&global_opts.current_input_device)),

    cons(list(symbol("current-error-device", NULL),
              port(&global_opts.current_error_device)),

    cons(list(symbol("current-filename", NULL),
              string(global_opts.current_filename)),

    cons(list(symbol("include-path", NULL),
              string(global_opts.include_path)),

    cons(list(symbol("library-path", NULL),
              string(global_opts.lib_path)),

    cons(list(symbol("startup-path", NULL),
              string(global_opts.startup_path)),

    cons(list(symbol("print-warnings", NULL),
              boolean(global_opts.warn)))))))))));
}

};
