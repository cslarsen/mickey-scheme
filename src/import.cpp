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

#include <dirent.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <cassert>
#include "mickey/cons.h"
#include "mickey/primitives.h"
#include "mickey/import.h"
#include "mickey/assertions.h"
#include "mickey/exceptions.h"
#include "mickey/print.h"
#include "mickey/options.h"
#include "mickey/eval.h"
#include "mickey/file-io.h"
#include "mickey/library.h"
#include "mickey/debug.h"
#include "mickey/system-features.h"
#include "mickey/cond-expand.h"

struct library_map_t {
  const char* library_name;
  const char* source_file;
};

void load_library_index();

/*
 * TODO: Create a small module that recurses lib-directory and adds all
 * libraries to a list of known libraries.  Use that to lookup library
 * names, and report errors (double library names, e.g.).
 */
static const char* library_index_file = "index.scm";
static library_map_t* library_map = NULL;
static const char** repl_libraries = NULL;

static std::string library_file(const std::string& basename);
static cons_t* verify_library_name(cons_t* p);

static struct library_t* import_lib(environment_t* target, const std::string& filename);
cons_t* proc_import(cons_t* p, environment_t* e);

/*
 * Library exports
 */
named_function_t exports_import[] = {
  {"import", proc_import, true},
  {NULL, NULL, false}
};

/*
 * Returns true if given library is supported, typically
 * supports_library("(scheme base)") returns true.
 */
bool supports_library(const char* s)
{
  load_library_index();

  for ( library_map_t *p = library_map; p->library_name != NULL; ++p ) {
    if ( !strcmp(s, p->library_name) )
      return true;
  }

  return false;
}

/*
 * Poor man's dirname().  For some reason beyond my understanding,
 * using dirname() char *s = strdup(filename); std::string r = dirname(s);
 * causes bugs.  Dunno why.
 *
 * So I'll just do it myself.
 */
static std::string sdirname(const char* filename)
{
  assert(filename != NULL);

  char *s = strdup(filename);
  for ( char *p = s+strlen(s); p>=s; --p )
    if ( *p == '/' || *p == '\\' ) {
      *p = '\0';
      break;
    }
  std::string r(s);
  free(s);
  return r;
}

static void invalid_index_format(const std::string& msg)
{
  raise(runtime_exception(format(
    "Invalid format for library index file: %s", msg.c_str())));
  exit(1);
}

/*
 * Return name of library defined in `file`, or empty string if file is not
 * a valid library file.
 */
std::string read_library_name(const std::string& file)
{
  TRY {
    program_t *p = parse(slurp(open_file(file)), null_environment());

    if ( symbol_name(caar(p->root)) != "define-library" )
      return "";

    cons_t *name = verify_library_name(cadar(p->root));
    return sprint(name);
  }
  CATCH(const exception_t& e) {
    int i=0; if ( e.what() ) ++i; // suppress unused variable warning
  }

  return "";
}

static std::vector<std::string> ls(const std::string& path)
{
  std::vector<std::string> ret;

  DIR *d = opendir(path.c_str());
  dirent *p = NULL;

  while ( (p = readdir(d)) != NULL )
    if ( p->d_type == DT_REG )
      ret.push_back(p->d_name);

  closedir(d);
  return ret;
}

bool ends_with(const std::string& s, const std::string& suffix)
{
  size_t slen = s.size();
  size_t xlen = suffix.size();

  if ( slen < xlen )
    return false;

  return slen<xlen? false : s.substr(slen-xlen, xlen) == suffix;
}

void scan_for_library_files()
{
  load_library_index();

  // Scan additional library directories
  for ( std::vector<std::string>::const_iterator i =
          global_opts.lib_path.begin();
        i != global_opts.lib_path.end();
        ++i )
  {
    // skip default library
    if ( !strcmp((*i).c_str(), global_opts.mickey_absolute_lib_path) )
      continue;

    // get all files in dir
    std::vector<std::string> files = ls(*i);

    // check if any of them are scheme libraries
    for ( size_t n=0; n<files.size(); ++n ) {
      if ( !ends_with(tolower(files[n]), ".scm") )
        continue;

      std::string name = read_library_name(files[n]);

      if ( name.empty() )
        continue;

      // entries in library map
      size_t len = 0;
      for ( library_map_t *p = library_map; p->library_name != NULL; ++p )
        ++len;

      // add an entry to end of library map
      library_map = static_cast<library_map_t*>(
          realloc(library_map, sizeof(library_map_t)*(len+2)));

      library_map[len].library_name = strdup(name.c_str());
      library_map[len].source_file = strdup(files[n].c_str());

      library_map[len+1].library_name = NULL;
      library_map[len+1].source_file = NULL;
    }
  }
}

void load_library_index()
{
  if ( library_map != NULL )
    return;

  std::string filename = library_file(library_index_file);
  environment_t *env = null_environment();
  program_t *p = parse(slurp(open_file(filename)), env);

  cons_t *index = p->root;

  if ( !pairp(index) || !symbolp(caar(index)) )
    invalid_index_format(filename + ": no list with symbols");

  for ( ; !nullp(index); index = cdr(index) ) {
    if ( symbol_name(caar(index)) == "define-library-index" ) {
      if ( library_map != NULL )
        invalid_index_format(filename + ": only one define-library-index allowed");

      if ( !listp(cdar(index)) ) {
        invalid_index_format(filename
          + ": define-library-index is not a list");
      }

      size_t len = length(cdar(index));
      library_map = (library_map_t*) malloc((1+len)*sizeof(library_map_t));

      size_t i = 0;
      for ( cons_t *lib = cdar(index); !nullp(lib); lib = cdr(lib), ++i ) {
        cons_t *name = caar(lib);
        cons_t *file = cadar(lib);

        if ( !listp(name) || !stringp(file) )
          invalid_index_format(filename + ": not list/string pair");

        library_map[i].library_name = strdup(sprint(name).c_str());
        library_map[i].source_file = strdup(file->string);
      }

      // important to signal end of list:
      library_map[i].library_name = NULL;
      library_map[i].source_file = NULL;

      continue;
    } else if ( symbol_name(caar(index)) == "define-repl-imports" ) {
      if ( repl_libraries != NULL )
        invalid_index_format(filename + ": only one define-repl-imports allowed");

      if ( !listp(cdar(index)) ) {
        invalid_index_format(filename
          + ": define-repl-imports is not a list");
      }

      size_t len = length(cdar(index));
      repl_libraries = (const char**) malloc((1+len)*sizeof(char*));

      const char **s = repl_libraries;
      for ( cons_t *lib = cdar(index); !nullp(lib); lib = cdr(lib), ++s ) {
        cons_t *name = car(lib);
        *s = strdup(sprint(name).c_str());
      }
      *s = NULL;
      continue;
    } else
      invalid_index_format(filename + ": unknown label "
        + sprint(caar(index)));
  }
}

void import(environment_t *e,
            named_function_t *p,
            const std::string& lib_name)
{
  if ( global_opts.verbose && !lib_name.empty() )
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
  eval(cons(symbol("begin"), p->root), p->globals);
}

/*
 * Add default libraries here.
 *
 */
void import_defaults(environment_t *e)
{
  load_library_index();

  const bool print =  global_opts.verbose &&
                     !global_opts.empty_repl_env &&
                      repl_libraries != NULL &&
                     *repl_libraries != NULL;

  if ( !global_opts.empty_repl_env ) {
    for ( const char** s = repl_libraries; *s != NULL; ++s ) {
      if ( print ) fprintf(stderr, "Importing %s\n", *s);
      merge(e, import_library(*s));
    }
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
    if ( type_of(car(q)) == INTEGER && car(q)->number.integer >= 0 )
      continue;

    raise(syntax_error("Invalid library name: " + sprint(p)));
  }

  return p;
}

static cons_t* include(cons_t* p, environment_t* e, const char* basedir)
{
  cons_t *code = list();
  /*
   * TODO: Paths should be relative (or under) the directory
   *       that the library file is in.
   */
  for ( ; !nullp(p); p = cdr(p) ) {
    assert_type(STRING, car(p));

    std::string file = format("%s/%s", basedir, car(p)->string);

    program_t *p = parse(slurp(open_file(file)), e);
    code = append(code, p->root);
  }

  return code;
}

static cons_t* include_ci(cons_t* p, environment_t* e, const char* basedir)
{
  bool flag = get_fold_case();
  set_fold_case(true);
  cons_t *code = include(p, e, basedir);
  set_fold_case(flag);
  return code;
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
 * - (cond-expand <cond-expand clause> ...)
 */
static library_t* define_library(cons_t* p, const char* file)
{
  library_t *r = gc_alloc_library();
  cons_t *exports = nil();

  // find current dir for resolving include and include-ci
  std::string curdir = sdirname(file);

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
      eval(splice(list(symbol("begin")),
                  include(body, r->internals, curdir.c_str())),
           r->internals);
      continue;
    }

    if ( s == "include-ci" ) {
      eval(splice(list(symbol("begin")),
                  include_ci(body, r->internals, curdir.c_str())),
           r->internals);
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
    if ( listp(car(p)) && length(car(p))==3 &&
         symbol_name(caar(p))=="rename" )
    {
      assert_type(SYMBOL, cadar(p));
      assert_type(SYMBOL, caddar(p));

      std::string internal_name = symbol_name(cadar(p));
      std::string external_name = symbol_name(caddar(p));

      r->exports->define(external_name,
                         r->internals->lookup(internal_name));
    } else if ( listp(car(p)) )
      raise(syntax_error("(export <spec> ...) only allows (rename x y)"));
    else if ( type_of(car(p)) == SYMBOL ) {
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
  // Try all library paths in order
  for ( std::vector<std::string>::const_iterator i =
          global_opts.lib_path.begin();
        i != global_opts.lib_path.end();
        ++i )
  {
    /*
     * TODO: Add more search heuristics here to find a file
     *       in library.
     */

    const std::string& library_path = *i;

    // first try library path
    std::string file = library_path + "/" + basename;

    // secondly, try include path
    if ( !file_exists(file.c_str()) )
      file = std::string(global_opts.include_path) + "/" + basename;

    if ( file_exists(file.c_str()) )
      return file;
  }

  raise(runtime_exception("Cannot find library file: " + basename));
  return "";
}

static library_t* import_lib(environment_t* target, const std::string& filename)
{
  if ( global_opts.verbose > 1 )
    fprintf(stderr, "Loading file %s\n", filename.c_str());

  program_t *p = parse(slurp(open_file(filename)), target);
  return define_library(p->root, filename.c_str());
}

/*
 * Read, parse and evaluate SCHEME SOURCE FILE in target environment.
 */
static void import(environment_t *target, const std::string& file)
{
  library_t *lib = import_lib(target, file);
  merge(target, lib->exports);
  lib->exports->outer = target;

  // TODO: Double-check that requested library name matches library name
  //       in file.
}

static void import_scheme_file(environment_t *r, const char* file)
{
  import(r, exports_import); // add (import)
  import(r, library_file(file));
}

static const char* dlext()
{
  // See http://sourceforge.net/p/predef/wiki/OperatingSystems/
#if defined(__gnu_linux__)
  return "so";
#elif defined(_WIN32)
  return "dll";
#elif defined(__APPLE__) && defined(__MACH__)
  return "so";
#else
# error "Unknown platform."
  return "so";
#endif
}

static void import_posix_dlopen(environment_t* r)
{
  /*
   * This is the only library we don't load dynamically from a scheme
   * file.  But we should load it dynamically from HERE via dlopen.
   *
   * Just ignore dlclose for now, the OS will take care of that as well
   * when process exit (we'll deal with resource handling like that later)
   * (TODO)
   */
  static void *lib = dlopen(library_file(format("libposix-dlopen.%s",
          dlext())).c_str(), RTLD_LAZY | RTLD_GLOBAL);

  if ( lib == NULL )
    raise(runtime_exception(format("(posix dlopen): %s", dlerror())));

  /*
   * Exported name on left, dlsym name on right
   */
  const char* sym[] = {
    "dlclose",         "proc_dlclose",
    "dlerror",         "proc_dlerror",
    "dlopen",          "proc_dlopen",
    "dlopen-internal", "proc_dlopen_internal",
    "dlopen-internal-determine-extension",
      "proc_dlopen_internal_determine_extension",
    "dlopen-determine-extension", "proc_dlopen_determine_extension",
    "dlopen-self",     "proc_dlopen_self",
    "dlsym",           "proc_dlsym",
    "dlsym-syntax",    "proc_dlsym_syntax",
    NULL};

  for ( const char** s = sym; *s; s += 2 ) {
    void *f = dlsym(lib, *(s+1));

    if ( f == NULL )
      raise(runtime_exception(format("(posix dlopen): %s", dlerror())));

    r->define(*s, reinterpret_cast<lambda_t>(f));
  }
}

environment_t* import_library(const std::string& name)
{
  load_library_index();
  environment_t* r = null_environment();

  /*
   * This library needs special treatment; all other libraries depend on it
   * to load dynamic shared object files.
   */
  if ( name == "(posix dlopen)" ) {
    import_posix_dlopen(r);
    return r;
  }

  /*
   * TODO: This lookup is O(n^2)-slow, but it will run so seldomly that it really
   * doesn't matter.  Can be done in O(n log n) or O(1) time, but at a cost
   * of algorithmic complexity.
   */
  for ( library_map_t* lib = library_map;
        lib->library_name != NULL; ++lib )
  {
    if ( name == lib->library_name ) {
      import_scheme_file(r, lib->source_file);
      return r;
    }
  }

  raise(runtime_exception("Unknown library: " + name));
  return NULL;
}

static environment_t* rename(environment_t* e, cons_t* ids)
{
  assert_type(PAIR, ids);

  // build a new environment and return it
  environment_t *r = null_environment();

  // TODO: Below code runs in slow O(n^2) time
  for ( dict_t::const_iterator i = e->symbols->begin();
        i != e->symbols->end(); ++i )
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

    r->symbols->operator[](name) = (*i).second;
  }

  return r;
}

static environment_t* prefix(environment_t* e, cons_t* identifier)
{
  assert_type(SYMBOL, identifier);

  // build a new environment and return it
  environment_t *r = null_environment();

  for ( dict_t::const_iterator i = e->symbols->begin();
        i != e->symbols->end(); ++i )
  {
    const std::string prefix = symbol_name(identifier);
    const std::string name = (*i).first;
    r->symbols->operator[](prefix + name) = (*i).second;
  }

  return r;
}

static environment_t* only(environment_t* e, cons_t* ids)
{
  assert_type(PAIR, ids);

  // build a new environment and return it
  environment_t *r = null_environment();

  for ( dict_t::const_iterator i = e->symbols->begin();
        i != e->symbols->end(); ++i )
  {
    std::string name = (*i).first;

    // only import specified names
    // TODO: Fix slow O(n^2) algo below
    for ( cons_t *id = ids; !nullp(id); id = cdr(id) ) {
      assert_type(SYMBOL, car(id));

      if ( symbol_name(car(id)) == name ) {
        r->symbols->operator[](name) = (*i).second;
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

  for ( dict_t::const_iterator i = e->symbols->begin();
        i != e->symbols->end(); ++i )
  {
    std::string name = (*i).first;

    // do not import specified name
    // TODO: Fix slow O(n^2) algo below
    for ( cons_t *id = ids; !nullp(id); id = cdr(id) ) {
      assert_type(SYMBOL, car(id));

      if ( symbol_name(car(id)) == name )
        goto DO_NOT_IMPORT;
    }

    r->symbols->operator[](name) = (*i).second;

DO_NOT_IMPORT:
    continue;
  }

  return r;
}

environment_t* import_set(cons_t* p)
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
   * TODO: Should we return the final environment, so we can easily run
   * cond-expand on it from outside define-library?  E.g., (cond-expand
   * (import (foo bar)))
   */
  return unspecified(nil());
}
