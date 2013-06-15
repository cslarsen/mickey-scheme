/*
 * UNIX wait for Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under the LGPL 2.1; see LICENSE
 */

#include <unistd.h>
#include "mickey.h"

typedef struct {
  const char* name;
  int value;
} _var;

static _var vars[] = {
#ifdef _SC_2_CHAR_TERM
  {"_SC_2_CHAR_TERM", _SC_2_CHAR_TERM},
#endif

#ifdef _SC_2_C_BIND
  {"_SC_2_C_BIND", _SC_2_C_BIND},
#endif

#ifdef _SC_2_C_DEV
  {"_SC_2_C_DEV", _SC_2_C_DEV},
#endif

#ifdef _SC_2_FORT_DEV
  {"_SC_2_FORT_DEV", _SC_2_FORT_DEV},
#endif

#ifdef _SC_2_FORT_RUN
  {"_SC_2_FORT_RUN", _SC_2_FORT_RUN},
#endif

#ifdef _SC_2_LOCALEDEF
  {"_SC_2_LOCALEDEF", _SC_2_LOCALEDEF},
#endif

#ifdef _SC_2_SW_DEV
  {"_SC_2_SW_DEV", _SC_2_SW_DEV},
#endif

#ifdef _SC_2_UPE
  {"_SC_2_UPE", _SC_2_UPE},
#endif

#ifdef _SC_2_VERSION
  {"_SC_2_VERSION", _SC_2_VERSION},
#endif

#ifdef _SC_ARG_MAX
  {"_SC_ARG_MAX", _SC_ARG_MAX},
#endif

#ifdef _SC_BC_BASE_MAX
  {"_SC_BC_BASE_MAX", _SC_BC_BASE_MAX},
#endif

#ifdef _SC_BC_DIM_MAX
  {"_SC_BC_DIM_MAX", _SC_BC_DIM_MAX},
#endif

#ifdef _SC_BC_SCALE_MAX
  {"_SC_BC_SCALE_MAX", _SC_BC_SCALE_MAX},
#endif

#ifdef _SC_BC_STRING_MAX
  {"_SC_BC_STRING_MAX", _SC_BC_STRING_MAX},
#endif

#ifdef _SC_CHILD_MAX
  {"_SC_CHILD_MAX", _SC_CHILD_MAX},
#endif

#ifdef _SC_CLK_TCK
  {"_SC_CLK_TCK", _SC_CLK_TCK},
#endif

#ifdef _SC_COLL_WEIGHTS_MAX
  {"_SC_COLL_WEIGHTS_MAX", _SC_COLL_WEIGHTS_MAX},
#endif

#ifdef _SC_EXPR_NEST_MAX
  {"_SC_EXPR_NEST_MAX", _SC_EXPR_NEST_MAX},
#endif

#ifdef _SC_IOV_MAX
  {"_SC_IOV_MAX", _SC_IOV_MAX},
#endif

#ifdef _SC_JOB_CONTROL
  {"_SC_JOB_CONTROL", _SC_JOB_CONTROL},
#endif

#ifdef _SC_LINE_MAX
  {"_SC_LINE_MAX", _SC_LINE_MAX},
#endif

#ifdef _SC_NGROUPS_MAX
  {"_SC_NGROUPS_MAX", _SC_NGROUPS_MAX},
#endif

#ifdef _SC_NPROCESSORS_CONF
  {"_SC_NPROCESSORS_CONF", _SC_NPROCESSORS_CONF},
#endif

#ifdef _SC_NPROCESSORS_ONLN
  {"_SC_NPROCESSORS_ONLN", _SC_NPROCESSORS_ONLN},
#endif

#ifdef _SC_OPEN_MAX
  {"_SC_OPEN_MAX", _SC_OPEN_MAX},
#endif

#ifdef _SC_PAGESIZE
  {"_SC_PAGESIZE", _SC_PAGESIZE},
#endif

#ifdef _SC_PHYS_PAGES
  {"_SC_PHYS_PAGES", _SC_PHYS_PAGES},
#endif

#ifdef _SC_RE_DUP_MAX
  {"_SC_RE_DUP_MAX", _SC_RE_DUP_MAX},
#endif

#ifdef _SC_SAVED_IDS
  {"_SC_SAVED_IDS", _SC_SAVED_IDS},
#endif

#ifdef _SC_STREAM_MAX
  {"_SC_STREAM_MAX", _SC_STREAM_MAX},
#endif

#ifdef _SC_TZNAME_MAX
  {"_SC_TZNAME_MAX", _SC_TZNAME_MAX},
#endif

#ifdef _SC_VERSION
  {"_SC_VERSION", _SC_VERSION},
#endif

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
  int name = lookup(symbol_name(car(p)).c_str());

  // symbol not recognized?
  if ( name < 0 )
    return boolean(false);

  int ret = sysconf(name);

  // name not recognized?
  if ( ret == -1 )
    return boolean(false);

  return integer(ret);
}

extern "C" cons_t* proc_sysconf_symbols(cons_t* p, environment_t*)
{
  assert_length(p, 0);

  cons_t *r = list();

  for ( int n=(sizeof(vars)/sizeof(_var))-1; n>=0; --n )
    if ( vars[n].name != NULL )
      r = cons(symbol(vars[n].name), r);

  return r;
}
