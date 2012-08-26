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

#include <stdio.h>

#ifdef USE_READLINE
# include <readline/readline.h>
# include <readline/history.h>
#endif

#include "mickey.h"
#include "cons.h"
#include "util.h"
#include "repl.h"
#include "parser.h"
#include "print.h"
#include "eval.h"
#include "heap.h"
#include "backtrace.h"
#include "options.h"
#include "libraries/scheme-base.h"
#include "module_import.h"
#include "exceptions.h"
#include "circular.h"

/*
 * Max items to display in REPL for circular lists.
 */
#define MAX_CIRCULAR_DISPLAY_ITEMS 33

// make env reachable by readline commands
static environment_t *global_env = NULL;

cons_t* proc_list_globals(cons_t*, environment_t *env)
{
  cons_t *r = list(NULL);

  for ( ; env != NULL; env = env->outer ) {
    dict_t::const_iterator i = env->symbols.begin();

    while ( i != env->symbols.end() ) {
      std::string n = (*i).first;
      r = append(r, cons(symbol(n.c_str(), env)));
      ++i;
    }
  }

  return r;
}

cons_t* proc_help(cons_t*, environment_t*)
{
  printf(
  "\n"
  "Welcome to Mickey, a scheme interperter that aims to correctly implement\n"
  "the whole of R7RS-small.\n"
  "\n"
  "In this REPL environment, you have several libraries preloaded for your\n"
  "convenience.  You can import additional libraries by invoking, e.g.,\n"
  "(import (scheme lazy)).\n"
  "\n"
  "Also, note that files executed from the command line have NO libraries\n"
  "imported by default, so you have to explicitly import typically\n"
  "(scheme base) and (scheme write).\n"
  "\n"
  "You will also stumble upon various bugs in Mickey, so please send bug\n"
  "reports.\n"
  "\n"
  );

  return nil();
}

named_function_t exports_repl[] = {
  {":list-globals", proc_list_globals, false},
  {"help", proc_help, false},
  {NULL, NULL, false}};

bool isprefix(const char* prefix, const char* fullstr)
{
  if ( strlen(prefix) > strlen(fullstr) )
    return false;

  while ( *prefix == *fullstr )
    ++prefix, ++fullstr;

  return *prefix == '\0';
}

#ifdef USE_READLINE
char** auto_complete(const char *s, int, int)
{
  /*
   * Note that readline will free up stuff for us, so we
   * don't have to use GC_MALLOC / copy_str here, but can safely
   * use both malloc / strdup.
   */

  static size_t last_hit = 0;

  cons_t *all_commands = proc_list_globals(NULL, global_env);
  size_t count = 0;

  // Count number of hits
  for ( cons_t *p = all_commands; !nullp(p); p = cdr(p) )
    count += (symbolp(car(p)) && isprefix(s, car(p)->symbol->name().c_str()));

  if ( count <= 1 ) {
    last_hit = 0;
    return NULL;
  }

  // Return hits; readline will free for us
  char** hits = (char**) malloc((1+count)*sizeof(char*));
  char** hit = hits;

  size_t hitno = 0;
  for ( cons_t *p = all_commands; !nullp(p); p = cdr(p) )
    if ( symbolp(car(p)) && isprefix(s, car(p)->symbol->name().c_str()) ) {
      ++hitno;
      if ( hitno >= last_hit )
        *hit++ = strdup(car(p)->symbol->name().c_str());
    }

  last_hit = hitno % count;

  *hit = NULL;
  return hits;
}

char* readline_auto_completion(const char* s, int state)
{
  /*
   * Regarding GC, see comment in auto_complete()
   */

  static char** commands = NULL;
  static int idx = 0;

  // Start-state; build list of completion hits
  if ( state == 0 ) {
    // get all commands
    cons_t *all_commands = proc_list_globals(NULL, global_env);
    size_t count = 0;

    // count number of hits
    for ( cons_t *p = all_commands; !nullp(p); p = cdr(p) )
      count += (symbolp(car(p)) &&
                 isprefix(s, car(p)->symbol->name().c_str()));
  
    // build actual hits; readline will (hopefully!) free for us
    // (but TODO don't count on it)
    commands = (char**) malloc((1+count)*sizeof(char*));
    char** command = commands;

    for ( cons_t *p = all_commands; !nullp(p); p = cdr(p) ) {
      if ( symbolp(car(p)) && isprefix(s, car(p)->symbol->name().c_str()) )
          *command++ = strdup(car(p)->symbol->name().c_str());
    }

    *command = NULL;
    idx = 0;
  } else {
    // bring up next hit; don't know if this is right
    if ( commands[idx] != NULL )
      ++idx;
  }

  return commands[idx];
}

void init_readline()
{
  // for .inputrc customization
  rl_readline_name = strdup("mickey");

  /*
   * I think there is a bug in GNU Readline; the signature
   * expected by the rl_completion_entry_function is a function
   * returning int, but when it's used in completion_matches,
   * it is a function returning char*.
   */
  rl_completion_entry_function = (int(*)(const char*,int)) readline_auto_completion;

  // hitting TAB will attempt auto completion
  rl_bind_key('\t', rl_complete);
}
#else
char* readline(const char* prompt)
{
  static char buf[1024];
  buf[0] = '\0';

  printf("%s", prompt);
  fflush(stdout);

  if ( !fgets(buf, sizeof(buf), stdin) )
    return NULL;

  return *buf? buf : NULL;
}
#endif // USE_READLINE

void print_banner(environment_t*)
{
  std::string readline_version;
  #ifdef USE_READLINE
    readline_version = format("Readline %d.%d",
      (rl_readline_version & 0xFF00) >> 8, rl_readline_version & 0x00FF);
  #endif

  printf("#| %-63s _\n", "");
  printf("   %-63s  \\\n", VERSION);
  printf("   %-63s  /\\\n", __VERSION__);
  printf("   %-63s /  \\_\n", readline_version.c_str());
}

int repl()
{
  global_env = null_import_environment();
  environment_t *env = global_env;

  print_banner(env);

  #ifdef USE_READLINE
  init_readline();
  #endif

  for(;;) {
    static char *input = reinterpret_cast<char*>(1);

    /*
     * input == 1 is the ugliest hack ever to
     * disoplay the imported_defaults message below,
     * and also because of ANOTHER hack, the home-brewed
     * exception system using longjumps.  I think I need to
     * rewrite... :)
     */
    if ( input != reinterpret_cast<char*>(1) ) {
      if ( (input = readline("#; mickey> ")) == NULL )
        break; // end of input stream

      if ( *trimr(input) == '\0' )
        continue; // empty command

      #ifdef USE_READLINE
      add_history(input);
      #endif

      if ( exception_raised() ) {
        backtrace();
        backtrace_clear();
        continue;
      }
    } else
      input = strdup("");

    TRY {
      /*
       * Must wrap import_defaults in try-catch
       */
      {
        static bool imported_defaults = false;
        if ( !imported_defaults ) {
          printf("\n");
          printf("   To quit, hit CTRL+D or type (exit).  Use (help) for an introduction.\n");
          printf("   Distributed under the LGPL 2.1; see LICENSE\n");
          printf("|#\n\n");
          import_defaults(env);
          import(env, exports_repl, "(scheme repl)");
          imported_defaults = true;
        }
      }

      program_t *p = parse(input, env);

      if ( p->parens < 0 )
        raise(runtime_exception(format(
          "parser: unbalanced parenthesis -> %ld", p->parens)));

      // Read until we have balanced parenthesis
      std::string s(input);
      while ( p->parens != 0 ) {
        if ( (input = readline("")) == NULL ) break;
        if ( *trimr(input) == '\0' ) continue;

        s += " ";
        s += input;
        delete p;

        #ifdef USE_READLINE
        free(input);
        input = NULL;
        #endif

        p = parse(s.c_str(), env);
      }

      #ifdef USE_READLINE
      if ( input ) free(input);
      #endif

      for ( cons_t *i = p->root; !nullp(i); i = cdr(i) ) {
        cons_t *result = eval(car(i), p->globals);

        if ( circularp(result) ) {
          fflush(stdout);
          fprintf(stderr, "Warning: List is circular\n");
          cons_t *l = list(), *end = l;

          for ( int n=0; n < MAX_CIRCULAR_DISPLAY_ITEMS; ++n ) {
            end->car = car(result);
            end->cdr = cons(nil());
            end = cdr(end);
            result = cdr(result);
          }

          end->car = symbol("...etc", null_environment());
          end->cdr = cons(nil());
          printf("%s\n", sprint(l).c_str());
        } else {
          std::string s = sprint(result);

          if ( !s.empty() )
            printf("%s\n", s.c_str());
        }
      }

      delete p;
    }
    CATCH (const exception_t& e) {
      if ( *e.what() != '\0' )
        fprintf(stderr, "%s\n", e.what());
      backtrace();
      backtrace_clear();
    }
  }

  printf("\n");
  return 0;
}
