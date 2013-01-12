/*
 * GPGME (GnuPG Made Easy) bindings for Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under any of LGPL v2.1, LGPL 3.0, GPL 2.0 or GPL 3.0
 *
 */

#include <gpgme.h>
#include <mickey.h>

static const char* version = NULL;

/*
 * GPGME authors have bundled their initialization code with the check
 * version function, so we need to call it.
 */
static void assert_gpgme_initialized(const char* required_version = NULL)
{
  if ( !version )
    version = gpgme_check_version(required_version);
}

extern "C" cons_t* proc_get_version(cons_t* p, environment_t*)
{
  assert_length(p, 0);
  assert_gpgme_initialized();
  return string(version);
}

extern "C" cons_t* proc_get_public_keys(cons_t* p, environment_t*)
{
  assert_length(p, 0);
  assert_gpgme_initialized();
  return list();
}

