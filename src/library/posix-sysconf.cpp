/*
 * UNIX wait for Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under the LGPL 2.1; see LICENSE
 */

#include <unistd.h>
#include "mickey.h"

static struct {
  const char* name;
  int value;
} vars[] = {
  {"_SC_2_CHAR_TERM", _SC_2_CHAR_TERM},
  {"_SC_2_C_BIND", _SC_2_C_BIND},
  {"_SC_2_C_DEV", _SC_2_C_DEV},
  {"_SC_2_FORT_DEV", _SC_2_FORT_DEV},
  {"_SC_2_FORT_RUN", _SC_2_FORT_RUN},
  {"_SC_2_LOCALEDEF", _SC_2_LOCALEDEF},
  {"_SC_2_SW_DEV", _SC_2_SW_DEV},
  {"_SC_2_UPE", _SC_2_UPE},
  {"_SC_2_VERSION", _SC_2_VERSION},
  {"_SC_ARG_MAX", _SC_ARG_MAX},
  {"_SC_BC_BASE_MAX", _SC_BC_BASE_MAX},
  {"_SC_BC_DIM_MAX", _SC_BC_DIM_MAX},
  {"_SC_BC_SCALE_MAX", _SC_BC_SCALE_MAX},
  {"_SC_BC_STRING_MAX", _SC_BC_STRING_MAX},
  {"_SC_CHILD_MAX", _SC_CHILD_MAX},
  {"_SC_CLK_TCK", _SC_CLK_TCK},
  {"_SC_COLL_WEIGHTS_MAX", _SC_COLL_WEIGHTS_MAX},
  {"_SC_EXPR_NEST_MAX", _SC_EXPR_NEST_MAX},
  {"_SC_IOV_MAX", _SC_IOV_MAX},
  {"_SC_JOB_CONTROL", _SC_JOB_CONTROL},
  {"_SC_LINE_MAX", _SC_LINE_MAX},
  {"_SC_NGROUPS_MAX", _SC_NGROUPS_MAX},
  {"_SC_NPROCESSORS_CONF", _SC_NPROCESSORS_CONF},
  {"_SC_NPROCESSORS_ONLN", _SC_NPROCESSORS_ONLN},
  {"_SC_OPEN_MAX", _SC_OPEN_MAX},
  {"_SC_PAGESIZE", _SC_PAGESIZE},
#ifdef _SC_PHYS_PAGES
  {"_SC_PHYS_PAGES", _SC_PHYS_PAGES},
#endif
  {"_SC_RE_DUP_MAX", _SC_RE_DUP_MAX},
  {"_SC_SAVED_IDS", _SC_SAVED_IDS},
  {"_SC_STREAM_MAX", _SC_STREAM_MAX},
  {"_SC_TZNAME_MAX", _SC_TZNAME_MAX},
  {"_SC_VERSION", _SC_VERSION},
  {NULL, 0}
};

static int lookup(const char* name)
{
  for ( int n=0; vars[n].name != NULL; ++n )
    if ( !strcmp(name, vars[n].name) )
      return vars[n].value;
  return -1;
}

extern "C" cons_t* proc_sysconf(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(SYMBOL, car(p));
  int value = lookup(symbol_name(car(p)).c_str());

  // not found?
  if ( value < 0 )
    return boolean(false);

  return integer(sysconf(value));
}
