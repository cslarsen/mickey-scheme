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
#include <libgen.h> // dirname
#include "mickey.h"
#include "repl.h"
#include "import.h"

/*
 * Environment variables:
 */
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

    fprintf(stderr, "\nError%s%s: %s\n\n",
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
    p->root = cons(symbol("begin"), p->root);
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
    p->lib_path.push_back(s+2);
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
  } else if ( s[0]=='-' && strlen(s+1)>1 ) {
    // several args
    char buf[3] = "-?";
    bool rest = false;
    for ( size_t i=1; s[i] != '\0'; ++i ) {
      buf[1] = s[i];
      if ( parse_option(buf, p) )
        rest = true;
    }
    if ( rest ) return rest; // rest is files
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

  set_default(&global_opts, argc, argv);

  /*
   * If there was no -L<path> option, set library path using either
   * environment variable or current working directory.
   */
  if ( global_opts.lib_path.empty() ) {
    if ( getenv(MICKEY_LIB) )
      add_lib_path(&global_opts, getenv(MICKEY_LIB));
    else {
      const char* s = strdup(format("%s/lib/",
                        global_opts.mickey_absolute_path).c_str());
      add_lib_path(&global_opts,s);
      global_opts.mickey_absolute_lib_path = s;
    }
  }

  std::vector<std::string> files;

  for ( int n=1; n<argc; ++n ) {
    if ( global_opts.eval_next ) {
      execute_string(argv[n]);
      global_opts.eval_next = false;
    } else if ( !rest_is_files && argv[n][0] == '-' ) {
      if ( argv[n][1] == '\0' )
        files.push_back("-"); // stdin
      else
        rest_is_files |= parse_option(argv[n], &global_opts);
    } else
      files.push_back(argv[n]);
  }

  scan_for_library_files();

  if ( !files.empty() ) {
    for ( size_t n=0; n<files.size(); ++n )
      execute(files[n].c_str());
  } else
    repl();

  return 0;
}
