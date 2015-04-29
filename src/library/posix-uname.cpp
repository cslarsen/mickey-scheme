/*
 * Example Mickey Scheme C library.
 *
 * Copyright (C) 2012 Christian Stigen Larsen
 * http://csl.sublevel3.org
 *
 * Compile with
 *
 * g++ -shared -fPIC -Iinclude mickey-uname.cpp -o libmickey-uname.so
 *
 * To use it,
 *
 * (import (posix dlopen))
 * (define lib (dlopen "libuname.so" 'lazy))
 * (define uname (dlsym lib "proc_uname"))
 * (display (uname))
 *
 */

#include <sys/utsname.h>
#include "mickey/mickey.h"

extern "C" cons_t* proc_uname(cons_t*, environment_t*)
{
  struct utsname p;

  if ( uname(&p) != 0 ) // could use errno as well
    return boolean(false);

  /*
   * Present values as a (sorted) a-list
   */
  return
    cons(cons(symbol("machine"), cons(string(p.machine))),
    cons(cons(symbol("nodename"), cons(string(p.nodename))),
    cons(cons(symbol("release"), cons(string(p.release))),
    cons(cons(symbol("sysname"), cons(string(p.sysname))),
    cons(cons(symbol("version"), cons(string(p.version))))))));
}
