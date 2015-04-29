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

#include "mickey/mickey.h"

extern "C" {

cons_t* global_options(cons_t* p, environment_t*)
{
  assert_length(p, 0);

  cons_t *paths = list();
  for ( std::vector<std::string>::const_iterator i =
          global_opts.lib_path.begin();
        i != global_opts.lib_path.end();
        ++i )
  {
    paths = append(paths, string((*i).c_str()));
  }

  /*
   * Only spit out useful options.  E.g., don't return argc/argv, because
   * (scheme command-line) can do that for you.
   */
  return
    cons(list(symbol("be-verbose"),
              boolean(global_opts.verbose)),

    cons(list(symbol("current-output-device"),
              port(&global_opts.current_output_device)),

    cons(list(symbol("current-input-device"),
              port(&global_opts.current_input_device)),

    cons(list(symbol("current-error-device"),
              port(&global_opts.current_error_device)),

    cons(list(symbol("current-filename"),
              string(global_opts.current_filename)),

    cons(list(symbol("include-path"),
              string(global_opts.include_path)),

    cons(list(symbol("library-paths"), list(paths)),

    cons(list(symbol("startup-path"),
              string(global_opts.startup_path)),

    cons(list(symbol("print-warnings"),
              boolean(global_opts.warn)))))))))));
}

};
