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

#include "mickey/types/environment_t.h"
#include "mickey/exceptions.h"
#include "mickey/primitives.h"
#include "mickey/print.h"

cons_t* environment_t::lookup_or_throw(const std::string& name) const
{
  cons_t *p = lookup(name);

  if ( p == NULL )
    raise(runtime_exception(
      format("Unbound definition: %s", name.c_str())));

  return p;
}

cons_t* environment_t::lookup(const std::string& name) const
{
  const environment_t *e = this;
  dict_t::const_iterator i;

  do {
    if ( (i = e->symbols.find(name)) != e->symbols.end() )
      return (*i).second;

  } while ( (e = e->outer) != NULL);

  return NULL;
}

struct cons_t* environment_t::define(const std::string& name, lambda_t f, bool syntactic)
{
  /*
   * TODO: Are redefinitions legal?
   */
  bool warn_mul_defs = false;

  if ( warn_mul_defs )
    fprintf(stderr, "WARNING: Already have a definition for %s\n",
      name.c_str());

  symbols[name] = closure(f, this, syntactic);
  return symbols[name];
}

struct cons_t* environment_t::define(const std::string& name, cons_t* body)
{
  /*
   * TODO: Are redefinitions legal?
   */
  bool warn_mul_defs = false;

  if ( warn_mul_defs )
    fprintf(stderr, "WARNING: Already have a definition for %s\n",
      name.c_str());

  return symbols[name] = body;
}

struct environment_t* environment_t::extend()
{
  environment_t *r = new environment_t();
  r->outer = this;
  return r;
}

environment_t* environment_t::outermost()
{
  environment_t *e = this;

  while ( e->outer != NULL )
    e = e->outer;

  return e;
}

int merge(environment_t *to, const environment_t *from)
{
  int r = 0;

  for ( dict_t::const_iterator i = from->symbols.begin();
        i != from->symbols.end(); ++i )
  {
    std::string name = (*i).first;

    // copy binding
    if ( to->lookup(name) == NULL ) {
      to->symbols[name] = (*i).second;
      ++r;
      continue;
    }

    // skip special symbol `import´
    if ( name == "import" )
      continue;

    /*
     * If we're inside a REPL, we are allowed to import
     * previously known bindings.
     *
     * TODO: Add a global option, something like
     *       OPTION_CAN_IMPORT_SAME_IDS_TWICE = true;
     */
    bool warn_multi_defs = true;

    if ( !warn_multi_defs ) {
      fprintf(stderr, "WARNING: We already have a definition for: %s\n",
        name.c_str());
      break;
    }
  }

  return r;
}
