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

#include "cons.h" // to_s(cons_t*)
#include "primitives.h"
#include "module_import.h"
#include "libraries/scheme-base.h"
#include "libraries/unix-dlopen.h"
#include "assertions.h"
#include "exceptions.h"
#include "print.h"
#include "options.h"
#include "eval.h"
#include "file-io.h"

/*
 * Library exports
 */
named_function_t exports_import[] = {
  {"import", proc_import, true},
  {NULL, NULL, false}
};

struct library_t {
  cons_t *name;
  environment_t *exports; // exported definitions
  environment_t *internals; // imported libs/defs

  library_t() :
    name(nil()),
    exports(null_environment()),
    internals(exports->extend())
  {
  }
};

void import(environment_t *e,
            named_function_t *p,
            const std::string& lib_name)
{
  if ( global_opts.verbose )
    fprintf(stderr, "Importing internal library %s\n",
        lib_name.c_str());

  for ( ; p->name && p->function; ++p )
    e->define(p->name, p->function, p->syntactic);
}

void load(const std::string& file, environment_t* target)
{
  if ( global_opts.verbose )
    fprintf(stderr, "Loading file %s\n", file.c_str());

  program_t *p = parse(slurp(open_file(file)), target);
  eval(cons(symbol("begin", p->globals),
            p->root), p->globals);
}

/*
 * Add default libraries here.
 *
 */
void import_defaults(environment_t *e)
{
  if ( !global_opts.empty_repl_env ) {
    merge(e, import_library("(scheme base)"));
    merge(e, import_library("(scheme cxr)"));
    merge(e, import_library("(scheme write)"));
    merge(e, import_library("(scheme char)"));
    merge(e, import_library("(scheme load)"));
    merge(e, import_library("(scheme repl)"));
  }
}

static cons_t* verify_library_name(cons_t* p)
{
  if ( !listp(p) )
    raise(syntax_error(format(
      "The library name must be a list, not %s",
        indef_art(to_s(type_of(p))).c_str())));

  /*
   * R7RS: <library name> is a list whose members are ...
   */
  for ( cons_t *q = p; !nullp(q); q = cdr(q) ) {
    // ... identifiers
    if ( type_of(car(q)) == SYMBOL )
      continue;

    // ... and exact nonnegative integers.
    if ( type_of(car(q)) == INTEGER && car(q)->integer >= 0 )
      continue;

    raise(syntax_error("Invalid library name: " + sprint(p)));
  }

  return p;
}

static cons_t* cond_expand(cons_t*, environment_t*)
{
  raise(unsupported_error("cond_expand is unsupported"));
  return nil();
}

static cons_t* include(cons_t* p, environment_t* e)
{
  cons_t *code = list();
  /*
   * TODO: Search the directory containing file is in
   */
  for ( ; !nullp(p); p = cdr(p) ) {
    assert_type(STRING, car(p));
    const char* filename = car(p)->string;

    program_t *p = parse(slurp(open_file(filename)), e);
    code = append(code, p->root);
  }

  return code;
}

static cons_t* include_ci(cons_t*, environment_t*)
{
  raise(unsupported_error("include-ci is unsupported"));
  return nil();
}

/*
 * Parse (define-library ...) form into given environment, with the
 * following format:
 *
 * (define-library <library name>
 *   <library declaration> ...)
 *
 * where <library declaration> is any of:
 *
 * - (export <export spec> ...)
 * - (import <import set> ...)
 * - (begin <command or definition> ...)
 * - (include <filename1> <filename2> ...)
 * - (include-ci <filename1> <filename2> ...)
 * - (conf-expand <cond-expand clause> ...)
 */
static library_t* define_library(cons_t* p, const char* file)
{
  library_t *r = new library_t();
  cons_t *exports = nil();

  // define-library
  if ( symbol_name(caar(p)) != "define-library" )
    raise(syntax_error(format(
      "Imported file does not begin with define-library: %s", file)));

  // <library name>
  r->name = verify_library_name(cadar(p));

  // A <library declaration> can be either ...
  for ( p = cdr(cdar(p)); !nullp(p); p = cdr(p) ) {
    cons_t *id   = caar(p);
    cons_t *body = cdar(p);
    std::string s = symbol_name(id);

    if ( s == "export" ) {
      exports = body;
      continue;
    }

    if ( s == "import" ) {
      // TODO: Make sure that proc_import does not override
      //       r->internals->outer
      proc_import(body, r->internals);
      continue;
    }

    if ( s == "begin" ) {
      eval(car(p), r->internals);
      continue;
    }

    if ( s == "include" ) {
      eval(include(body, r->internals), r->internals);
      continue;
    }

    if ( s == "include-ci" ) {
      eval(include_ci(body, r->internals), r->internals);
      continue;
    }

    if ( s == "cond-expand" ) {
      eval(cond_expand(body, r->internals), r->internals);
      continue;
    }
  }

  // copy exports into exports-environemnt
  for ( p = exports; !nullp(p); p = cdr(p) ) {

    // handle renaming
    if ( pairp(car(p)) ) {
      assert_type(SYMBOL, caar(p));
      assert_type(SYMBOL, cadr(p));

      std::string internal_name = symbol_name(caar(p));
      std::string external_name = symbol_name(cadr(p));

      r->exports->define(external_name,
                         r->internals->lookup(internal_name));
    } else if ( type_of(car(p)) == SYMBOL ) {
      r->exports->define(symbol_name(car(p)),
                         r->internals->lookup(symbol_name(car(p))));
    } else
      raise(syntax_error(
        "(export <spec> ...) requires <spec> to be "
        "either an identifier or a pair of them."));
  }

  return r;
}

/*
 * Use heuristics to find file in our library.
 */
static std::string library_file(const std::string& basename)
{
  const std::string library_path = global_opts.lib_path;

  /*
   * TODO: Add more search heuristics here to find a file
   *       in library.
   */

  // first try library path
  std::string file = library_path + "/" + basename;

  // secondly, try include path
  if ( !file_exists(file.c_str()) )
    file = std::string(global_opts.include_path) + "/" + basename;

  // no cigar
  if ( !file_exists(file.c_str()) )
    raise(runtime_exception("Cannot find library file: " + file));

  return file;
}

/*
 * Read, parse and evaluate SCHEME SOURCE FILE in target environment.
 */
static void import(environment_t *target, const std::string& filename)
{
  if ( global_opts.verbose )
    fprintf(stderr, "Loading file %s\n", filename.c_str());

  program_t *p = parse(slurp(open_file(filename)), target);
  library_t *lib = define_library(p->root, filename.c_str());
  merge(target, lib->exports);
  lib->exports->outer = target;

  // TODO: Double-check that request library name matches library name
  //       in file.
}

static void import_scheme_file(environment_t *r, const char* file)
{
  import(r, exports_import); // add (import)
  import(r, library_file(file));
}

environment_t* import_library(const std::string& name)
{
  environment_t* r = null_environment();

  if ( global_opts.verbose )
    fprintf(stderr, "Import library %s\n", name.c_str());

  /*
   * TODO: Create a small module that recurses lib-directory and
   *       adds all libraries to a list of known libraries.
   *       Use that to lookup library names, and report errors
   *       (double library names, e.g.).
   */

  if ( name == "(scheme base)" )
    import(r, library_file("scheme/base.scm")); // Scheme source code

  else if ( name == "(scheme math)" )
    import_scheme_file(r, "scheme/math.scm");

  else if ( name == "(scheme char)" )
    import_scheme_file(r, "scheme/char.scm");

  else if ( name == "(scheme cxr)" )
    import_scheme_file(r, "scheme/cxr.scm");

  else if ( name == "(scheme lazy)" )
    import_scheme_file(r, "scheme/lazy.scm");

  else if ( name == "(scheme write)" )
    import_scheme_file(r, "scheme/write.scm");

  else if ( name == "(scheme load)" )
    import_scheme_file(r, "scheme/load.scm");

  else if ( name == "(scheme repl)" )
    import_scheme_file(r, "scheme/repl.scm");

  else if ( name == "(scheme process-context)" )
    import_scheme_file(r, "scheme/process-context.scm");

  else if ( name == "(mickey environment)" )
    import_scheme_file(r, "mickey/environment.scm");

  else if ( name == "(mickey internals)" )
    import_scheme_file(r, "mickey/internals.scm");

  else if ( name == "(unix uname)" )
    import_scheme_file(r, "unix/uname.scm");

  else if ( name == "(mickey misc)" )
    import_scheme_file(r, "mickey/misc.scm");

  else if ( name == "(mickey library)" )
    import_scheme_file(r, "mickey/library.scm");

  else if ( name == "(unix dlopen)" ) {
    /*
     * This is the only library we don't need to load dynamically from a
     * scheme file.  But (TODO) we should load it dynamically from HERE via
     * dlopen.
     */
    import(r, exports_dlopen, name);
  }

  else
    raise(runtime_exception("Unknown library: " + name));

  return r;
}

static environment_t* rename(environment_t* e, cons_t* ids)
{
  assert_type(PAIR, ids);

  // build a new environment and return it
  environment_t *r = null_environment();

  // TODO: Below code runs in slow O(n^2) time
  for ( dict_t::const_iterator i = e->symbols.begin();
        i != e->symbols.end(); ++i )
  {
    std::string name = (*i).first;

    // find new name
    for ( cons_t *id = ids; !nullp(id); id = cdr(id) ) {
      assert_type(PAIR, car(id));
      assert_type(SYMBOL, caar(id));
      assert_type(SYMBOL, cadar(id));
      if ( symbol_name(caar(id)) == name ) {
        name = symbol_name(cadar(id));
        break;
      }
    }

    r->symbols[name] = (*i).second;
  }

  return r;
}

static environment_t* prefix(environment_t* e, cons_t* identifier)
{
  assert_type(SYMBOL, identifier);

  // build a new environment and return it
  environment_t *r = null_environment();

  for ( dict_t::const_iterator i = e->symbols.begin();
        i != e->symbols.end(); ++i )
  {
    const std::string prefix = symbol_name(identifier);
    const std::string name = (*i).first;
    r->symbols[prefix + name] = (*i).second;
  }

  return r;
}

static environment_t* only(environment_t* e, cons_t* ids)
{
  assert_type(PAIR, ids);

  // build a new environment and return it
  environment_t *r = null_environment();

  for ( dict_t::const_iterator i = e->symbols.begin();
        i != e->symbols.end(); ++i )
  {
    std::string name = (*i).first;

    // only import specified names
    // TODO: Fix slow O(n^2) algo below
    for ( cons_t *id = ids; !nullp(id); id = cdr(id) ) {
      assert_type(SYMBOL, car(id));

      if ( symbol_name(car(id)) == name ) {
        r->symbols[name] = (*i).second;
        break;
      }
    }
  }

  return r;
}

static environment_t* except(environment_t* e,  cons_t* ids)
{
  assert_type(PAIR, ids);

  // build a new environment and return it
  environment_t *r = null_environment();

  for ( dict_t::const_iterator i = e->symbols.begin();
        i != e->symbols.end(); ++i )
  {
    std::string name = (*i).first;

    // do not import specified name
    // TODO: Fix slow O(n^2) algo below
    for ( cons_t *id = ids; !nullp(id); id = cdr(id) ) {
      assert_type(SYMBOL, car(id));

      if ( symbol_name(car(id)) == name )
        goto DO_NOT_IMPORT;
    }

    r->symbols[name] = (*i).second;

DO_NOT_IMPORT:
    continue;
  }

  return r;
}

static environment_t* import_set(cons_t* p)
{
  std::string s = symbol_name(car(p));

  /*
   * Each import set can be either of:
   */

  // (rename <import set> (<identifier1> <identifier2>) ...)
  if ( s == "rename" )
    return rename(import_set(cadr(p)), cddr(p));

  // (prefix <import set> <identifier>)
  else if ( s == "prefix" )
    return prefix(import_set(cadr(p)), caddr(p));

  // (only <import set> <identifier> ...)
  else if ( s == "only" )
    return only(import_set(cadr(p)), cddr(p));

  // (except <import set> <identifier> ...)
  else if ( s == "except" )
    return except(import_set(cadr(p)), cddr(p));

  // <library name>
  else if ( !s.empty() )
    return import_library(sprint(p));

  raise(runtime_exception("Unknown import set: " + sprint(p)));
  return NULL;
}

cons_t* proc_import(cons_t* p, environment_t* e)
{
  assert_length_min(p, 1);
  assert_type(PAIR, car(p));

  /*
   * Handle all import sets in (import <import set> ...)
   */
  for ( ; !nullp(p); p = cdr(p) ) {
    environment_t *impenv = import_set(car(p));

    /*
     * Now we need to bring the imported environment to the environment,
     * so that the new definitions are available there.
     *
     * We do this by copying the definitions.
     */
    merge(e, impenv);

    /*
     * But we also need to connect the lower level imported environment to
     * definitions found in its outer environment.
     *
     * This is because the exported functions in impenv must be able to see
     * definitions in the toplevel, controlling, environment.
     *
     * Consider the (mickey environment) module, which has a "syntactic"
     * procedure bound?.
     *
     * If we (import (scheme write)) then we get the procedure display.  But
     * if we now (import (mickey environment)) and call (bound? display)
     * then bound? will not be able to see any definition of display, and
     * will wrongly return #f.
     *
     * Note that I'm not entirely certain that this is the correct way of
     * handling things, since closures must be evaluated in the environment
     * they were defined in.
     *
     * TODO: Think hard about this and write some tests.
     *
     * Note that this behaviour might be different for libraries that are
     * imported as scheme source code.  They must be first evaluated in
     * their own closed environment (to bind definitions) before being
     * connected to the outer one.
     *
     * I think what we need is a global pointer to the ACTUAL top-level
     * environment.
     *
     */
    impenv->outer = e;
  }

  /*
   * TODO: Should we return the final environment,
   *       so we can easily run cond-expand on it?
   */
  return nil();
}
