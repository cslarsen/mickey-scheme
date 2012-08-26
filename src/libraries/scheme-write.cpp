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

extern "C" cons_t* proc_display(cons_t *p, environment_t*)
{
  assert_length(p, 1, 2);

  /*
   * Get port to write to.
   *
   * TODO: Should we check if the file descriptor
   *       is open?
   */
  port_t* port = &global_opts.current_output_device;

  if ( length(p) == 2 ) {
    assert_type(PORT, cadr(p));
    port = cadr(p)->port;
  }

  /*
   * TODO: Implement display in terms of (write) and
   *       use tail call elimination to be able to
   *       endlessly print circular lists.
   */
  std::string s = print(car(p));
  fwrite(s.c_str(), s.length(), 1, port->file());

  return nil();
}
