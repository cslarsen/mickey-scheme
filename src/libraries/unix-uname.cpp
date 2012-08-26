/*
 * Example Mickey Scheme C library.
 *
 * Copyright (C) 2012 Christian Stigen Larsen
 * http://csl.sublevel3.org
 *
 * Compile with
 *
 * llvm-g++ -shared -Iinclude mickey-uname.cpp
 *
 * To use it,
 *
 * (import (unix dlopen))
 * (define lib (dlopen "libuname.so" 'lazy))
 * (define uname (dlsym lib "proc_uname"))
 * (display (uname))
 *
 */

#include <sys/utsname.h>
#include "mickey.h"

extern "C" cons_t* proc_uname(cons_t*, environment_t*)
{
  struct utsname p;

  if ( uname(&p) != 0 ) // could use errno as well
    return boolean(false);

  // return values as an a-list
  return
    cons(cons(symbol("sysname", NULL), cons(string(p.sysname))),
    cons(cons(symbol("nodename", NULL), cons(string(p.nodename))),
    cons(cons(symbol("release", NULL), cons(string(p.release))),
    cons(cons(symbol("version", NULL), cons(string(p.version))),
    cons(cons(symbol("machine", NULL), cons(string(p.machine))))))));
}
