/*
 * UNIX wait for Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under the LGPL 2.1; see LICENSE
 */

#include "mickey.h"


extern "C" cons_t* proc_implementation_name(cons_t* p, environment_t*)
{
  assert_length(p, 0);
  return string(MICKEY_NAME);
}

extern "C" cons_t* proc_implementation_version(cons_t* p, environment_t*)
{
  assert_length(p, 0);
  return string(format("%d.%d", MICKEY_VERSION_MAJOR,
                                MICKEY_VERSION_MINOR).c_str());
}

/*
 * Query sizes of basic C data types.
 *
 */
extern "C" cons_t* proc_sizeof(cons_t* p, environment_t*)
{
  static struct {
    const char* name;
    size_t size;
  } sizes[] = {
    {"char", sizeof(char)},
    {"int", sizeof(int)},
    {"long", sizeof(long)},
    {"pointer", sizeof(void*)}, // shorthand
    {"short", sizeof(short)},
    {"void*", sizeof(void*)},
    {NULL, 0}
  };

  assert_length(p, 1);
  assert_type(SYMBOL, car(p));

  std::string s = symbol_name(car(p));

  for ( size_t n=0; sizes[n].name != NULL; ++n )
    if ( s == sizes[n].name )
      return integer(sizes[n].size);

  // not found
  return boolean(false);
}
