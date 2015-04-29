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

#include "mickey/mickey.h"

extern "C" {

cons_t* proc_read_from_port(cons_t* p, environment_t* e)
{
  assert_length(p, 1);
  assert_type(PORT, car(p));

  port_t* port = car(p)->port;

  FILE *f = port->file();
  std::string s;
  int ch = ' ';

  while ( ch != '\n' && !feof(f) ) {
    s += ch;
    ch = fgetc(f);
  }

  program_t *prog = parse(s, e);
  return car(prog->root);

  /*
   * TODO: If you do this:
   *
   * > (read (current-input-port))
   * (+ 1 2 3) (* 4 5 6)
   *
   * it should RETURN '(+ 1 2 3) and
   * EVALUATE (* 4 5 6) the reader should
   * stop parsing as soon as it is finished,
   * so the loop above should check balanced
   * parens. When balanced, it should bail out.
   */
}

}
