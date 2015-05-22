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

#include <dlfcn.h>
#include <libgen.h>
#include "mickey/cons.h"
#include "mickey/library/posix-dlopen.h"

extern "C" {

/*
 * Define an arbitrary type tag to use to discern
 * between different pointers.
 */
#define TYPE_TAG "dynamic-shared-library-handle"

/*
 * Symbols we will use for the RTLD_* mode flags.
 */
#define SYMBOL_RTLD_LAZY   "lazy"
#define SYMBOL_RTLD_NOW    "now"
#define SYMBOL_RTLD_GLOBAL "global"
#define SYMBOL_RTLD_LOCAL  "local"

named_function_t exports_dlopen[] = {
  {"dlclose", proc_dlclose, false},
  {"dlerror", proc_dlerror, false},
  {"dlopen", proc_dlopen, false},
  {"dlopen-determine-extension", proc_dlopen_determine_extension, false},
  {"dlopen-internal", proc_dlopen_internal, false},
  {"dlopen-internal-determine-extension",
    proc_dlopen_internal_determine_extension, false},
  {"dlopen-self", proc_dlopen_self, false},
  {"dlsym", proc_dlsym, false},
  {"dlsym-syntax", proc_dlsym_syntax, false},
  {NULL, NULL, false}};

/*
 * Platform's native dynamic library extension.
 * TODO: Is there a tool for doing stuff like this?
 *
 * See http://sourceforge.net/p/predef/wiki/OperatingSystems/ for macros.
 */
  static const char dlextension[] =
#if defined(__gnu_linux__)
    "so";
#elif defined(__APPLE__) && defined(__MACH__)
    "so";
#elif defined(_WIN32)
    "dll";
#else
# error "Unable to determine dynamic library extension for unknown platform."
    "so";
#endif

/*
 * ... and the function to parse it
 */
static int parse_dlopen_mode(cons_t* p)
{
  int mode = 0;

  for ( cons_t *m = p; !nullp(m); m = cdr(m) ) {
    std::string n = symbol_name(car(m));

         if ( n == SYMBOL_RTLD_LAZY )   mode |= RTLD_LAZY;
    else if ( n == SYMBOL_RTLD_NOW )    mode |= RTLD_NOW;
    else if ( n == SYMBOL_RTLD_GLOBAL ) mode |= RTLD_GLOBAL;
    else if ( n == SYMBOL_RTLD_LOCAL )  mode |= RTLD_LOCAL;
    else {
      raise(runtime_exception(format(
        "Unknown dlopen mode parameter %s --- "
        "available modes are %s %s %s %s", n.c_str(),
          SYMBOL_RTLD_LAZY, SYMBOL_RTLD_NOW,
          SYMBOL_RTLD_GLOBAL, SYMBOL_RTLD_LOCAL)));
    }
  }

  return mode;
}

cons_t* proc_dlclose(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer(TYPE_TAG, car(p));
  void *handle = car(p)->pointer->value;
  return boolean(dlclose(handle) == 0);
}

cons_t* proc_dlerror(cons_t*, environment_t*)
{
  const char* s = dlerror();
  return string(s!=NULL? s : "");
}

/*
 * Signature: (dlopen <filename> <mode options> ...)
 *
 * Options must be symbols with names lazy now global local.  These
 * correspond to the RTLD_LAZY, RTLD_NOW, RTLD_GLOBAL and RTLD_LOCAL mode
 * options.  If you specify several options, they will be bitwise OR'ed
 * together.
 *
 */
cons_t* proc_dlopen(cons_t* p, environment_t*)
{
  assert_length_min(p, 1);
  assert_type(STRING, car(p));

  void *h = dlopen(car(p)->string, parse_dlopen_mode(cdr(p)));

  return h!=NULL?  pointer(gc_alloc_pointer(TYPE_TAG, h)) : boolean(false);
}

/*
 * Signature: (dlopen-determine-extension <basename> <mode options> ...)
 *
 * This is a wrapper around (dlopen) that accepts a filename without an
 * extension like .dll, .so or .dylib. Based on compilation flags, it will add
 * the proper extension.
 */
cons_t* proc_dlopen_determine_extension(cons_t* p, environment_t* env)
{
  assert_length_min(p, 1);
  assert_type(STRING, car(p));

  const std::string base = sbasename(car(p)->string) + "." + dlextension;
  p = deep_copy(p);
  car(p)->string = base.c_str();
  return proc_dlopen(p, env);
}

/*
 * Signature: (dlopen <mode options> ...)
 *
 * Opens the currently running code.
 */
cons_t* proc_dlopen_self(cons_t* p, environment_t*)
{
  int mode = RTLD_GLOBAL | RTLD_LAZY; // default

  if ( length(p) > 0 )
    mode = parse_dlopen_mode(cdr(p));

  void *h = dlopen(NULL, mode);

  return h!=NULL?  pointer(gc_alloc_pointer(TYPE_TAG, h)) : boolean(false);
}

/*
 * Same as proc_dlopen, but will load from Mickey Scheme's library
 * directory.  It's a utility function to load default libraries without
 * having to guess the path.
 */
cons_t* proc_dlopen_internal(cons_t* p, environment_t*)
{
  assert_length_min(p, 1);
  assert_type(STRING, car(p));

  const std::string base = sbasename(car(p)->string);

  for ( size_t n=0; n < global_opts.lib_path.size(); ++n ) {

    /*
     * File will point to Mickey Scheme's location and then in the /lib/
     * subdirectory
     */
    const std::string path = global_opts.lib_path[n];
    const std::string file = path + "/" + base;

    if ( file_exists(file.c_str()) ) {
      void *h = dlopen(file.c_str(), parse_dlopen_mode(cdr(p)));

      return h!=NULL?  pointer(gc_alloc_pointer(TYPE_TAG, h)) : boolean(false);
    }
  }

  raise(runtime_exception(format("Could not find shared object file: %s",
    base.c_str())));
  return nil();
}

cons_t* proc_dlopen_internal_determine_extension(cons_t* p, environment_t* env)
{
  assert_length_min(p, 1);
  assert_type(STRING, car(p));

  const std::string base = sbasename(car(p)->string) + "." + dlextension;
  p = deep_copy(p);
  car(p)->string = base.c_str();
  return proc_dlopen_internal(p, env);
}

static lambda_t dlsym_helper(cons_t* p)
{
  assert_length(p, 2, 3);
  assert_pointer(TYPE_TAG, car(p));
  assert_type(STRING, cadr(p));

  pointer_t *handle = car(p)->pointer;
  const char* name = cadr(p)->string;

  return reinterpret_cast<lambda_t>(dlsym(handle->value, name));
}

/*
 * Takes a handle, a function name and an optional environment
 * (default is current environment) and returns a closure
 * of the given function and environment, or #f if operation
 * failed.
 *
 * (dlsym <handle> <name> <environment>?)
 *
 */
cons_t* proc_dlsym(cons_t* p, environment_t* current)
{
  environment_t *e = current;

  if ( length(p) == 3 ) {
    assert_type(ENVIRONMENT, caddr(p));
    e = caddr(p)->environment;
  }

  lambda_t f = dlsym_helper(p);
  return f != NULL ? closure(f, e) : boolean(false);
}

/*
 * Same as dlsym, but returns a syntactic closure, meaning that function
 * arguments are not evaluated before invocation.
 */
cons_t* proc_dlsym_syntax(cons_t* p, environment_t* current)
{
  environment_t *e = current;

  if ( length(p) == 3 ) {
    assert_type(ENVIRONMENT, caddr(p));
    e = caddr(p)->environment;
  }

  lambda_t f = dlsym_helper(p);
  return f != NULL ? closure(f, e, true) : boolean(false);
}

}
