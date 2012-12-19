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

#include <stdlib.h>
#include <stdio.h>
#include "mickey.h"

extern "C" {

/*
 * (fopen <filename> <mode>)
 *
 * returns pointer to FILE*; called file-obj
 */
cons_t* proc_fopen(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(STRING, car(p));
  assert_type(STRING, cadr(p));

  const char* filename = car(p)->string;
  const char* mode = cadr(p)->string;
  FILE *f = fopen(filename, mode);

  if ( f == NULL ) {
    raise(runtime_exception(format(
      "Could not open '%s' with mode '%s'",
      filename, mode)));
  }

  return pointer("FILE*", f);
}

/*
 * (freopen <filename>) <mode> <file-obj>)
 */
cons_t* proc_freopen(cons_t* p, environment_t*)
{
  assert_length(p, 3);
  assert_type(STRING, car(p));
  assert_type(STRING, cadr(p));
  assert_pointer("FILE*", caddr(p));

  const char* filename = car(p)->string;
  const char* mode = cadr(p)->string;
  FILE* f = reinterpret_cast<FILE*>(caddr(p)->pointer->value);

  return !freopen(filename, mode, f)? nil() : boolean(false);
}

/*
 * (fflush <file-obj>)
 */
cons_t* proc_fflush(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer("FILE*", car(p));
  FILE *f = reinterpret_cast<FILE*>(car(p)->pointer->value);
  return !fflush(f)? nil() : boolean(false);
}

/*
 * (fclose <file-obj>)
 */
cons_t* proc_fclose(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer("FILE*", car(p));
  FILE *f = reinterpret_cast<FILE*>(car(p)->pointer->value);
  return !fclose(f)? nil() : boolean(false);
}

/*
 * (fread <size> <file-obj>)
 */
cons_t* proc_fread(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(INTEGER, car(p));
  assert_pointer("FILE*", cadr(p));

  if ( car(p)->number.integer < 0 )
    raise(runtime_exception("Size must be a positive integer"));

  size_t size = static_cast<size_t>(car(p)->number.integer);
  FILE *f = reinterpret_cast<FILE*>(cadr(p)->pointer->value);

  char *buf = static_cast<char*>(malloc(sizeof(char)*(size+1)));
  buf[size] = '\0';

  size_t r = fread(buf, sizeof(char), size, f);
  cons_t* ret = list(integer(r), string(buf));
  free(buf);
  return ret;
}

cons_t* proc_feof(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer("FILE*", car(p));
  FILE *f = reinterpret_cast<FILE*>(car(p)->pointer->value);
  return boolean(feof(f) != 0);
}

}
