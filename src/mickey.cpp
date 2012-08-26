/*
 * Mickey Scheme
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *                                                          
 */

#include <libgen.h> // dirname
#include "options.h"
#include "repl.h"
#include "parser.h"
#include "eval.h"
#include "print.h"
#include "heap.h"
#include "backtrace.h"
#include "module_import.h"
#include "exceptions.h"
#include "mickey.h"
#include "options.h"

#define MICKEY_LIB "MICKEY_LIB"

void version();
void help();

void execute(const char* file)
{
  TRY {
    environment_t *env = null_import_environment();
    reset_for_programs(&global_opts, file);
    load(file, env);
  }
  CATCH (const exception_t& e) {
    const char* file = global_opts.current_filename;
    bool has_file = file && strcmp(file, "-");

    /*
     * Finish any unfinished printing before
     * writing errors.
     */
    fflush(stdout);

    fprintf(stderr, "\nError%s%s: %s\n",
      has_file? " in " : "",
      has_file? file : "",
      e.what());

    backtrace();
    backtrace_clear();
    exit(1);
  }
}

void execute_string(const char* s)
{
  TRY {
    environment_t *env = null_import_environment();
    reset_for_programs(&global_opts, NULL);

    program_t *p = parse(s, env);
    p->root = cons(symbol("begin", p->globals), p->root);
    printf("%s\n", sprint(eval(p->root, env)).c_str());
  }
  CATCH (const exception_t& e) {
    fprintf(stderr, "\nError: %s\n", e.what());
    backtrace();
    backtrace_clear();
    exit(1);
  }
}

// return true if rest of parameters are files
bool parse_option(const char* s, struct options_t* p)
{
  #define ARGHIT(_short, _long) (!strcmp(s, _short) || !strcmp(s, _long))

  if ( !strcmp(s, "--") ) {
    return true;
  } else if ( !strncmp(s, "-I", 2) && strlen(s) > 2 ) {
    p->include_path = s+2;
  } else if ( !strncmp(s, "-L", 2) && strlen(s) > 2 ) {
    p->lib_path = s+2;
  } else if ( !strcmp(s, "-") ) {
    // TODO: read from standard input
  } else if ( ARGHIT("-v", "--verbose") ) {
    p->verbose = true;
  } else if ( ARGHIT("-V", "--version") ) {
    version();
    exit(0);
  } else if ( ARGHIT("-h", "--help") ) {
    help(); 
    exit(0);
  } else if ( ARGHIT("-e", "--eval") ) {
    p->eval_next = true;
  } else if ( ARGHIT("-z", "--zero-env") ) {
    p->empty_repl_env = true;
  } else {
    fprintf(stderr, "Unknown option: %s\n\n", s);
    help();
    exit(1);
  }

  return false;
}

void help()
{
  printf("Usage: mickey [ option(s) ] [ file(s) | - ]\n"
    "\n"
    "Options:\n"
    "  -             Read program from standard input\n"
    "  --            Rest of arguments are files, i.e. files can now begin with -\n"
    "  -e --eval     Execute expression and print result\n"
    "  -h --help     Print help\n"
    "  -I<path>      Set include path for (load)\n"
    "  -L<path>      Set location for library imports\n"
    "  -V --version  Print version\n"
    "  -v --verbose  Verbose operation\n"
    "  -z --zero-env Start REPL with only (import) defined\n"
    "\n");
}

void version()
{
  printf("%s\n", VERSION);
  printf("Compiler version: %s\n", __VERSION__);
}

int main(int argc, char** argv)
{
  bool rest_is_files = false; // used with option `--`
  bool run_repl = true;

  set_default(&global_opts, argc, argv);

  /*
   * If there was no -L<path> option, set library path using either
   * environment variable or current working directory.
   */
  if ( strlen(global_opts.lib_path) == 0 ) {
    if ( getenv(MICKEY_LIB) )
      set_lib_path(&global_opts, getenv(MICKEY_LIB));
    else
      set_lib_path(&global_opts,
        format("%s/lib/", global_opts.mickey_absolute_path).c_str());
  }

  for ( int n=1; n<argc; ++n ) {
    if ( global_opts.eval_next ) {
      execute_string(argv[n]);
      global_opts.eval_next = false;
      run_repl = false;
    } else if ( !rest_is_files && argv[n][0] == '-' ) {
      if ( argv[n][1] == '\0' )
        execute("-"); // stdin
      else
        rest_is_files |= parse_option(argv[n], &global_opts);
    } else {
      execute(argv[n]); // file
      run_repl = false;
    }
  }

  if ( run_repl )
    repl();

  return 0;
}
