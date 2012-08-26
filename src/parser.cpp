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

#include "parser.h"
#include "types.h"
#include "util.h"
#include "print.h"
#include "exceptions.h"

cons_t* type_convert(const char* token, environment_t* env)
{
  if ( isfloat(token) )
    return decimal(to_f(token));

  if ( isinteger(token) )
    return integer(to_i(token));

  if ( isstring(token) )
    return parse_string(token);

  if ( isatom(token) )
    return symbol(token, env);

  if ( isbool(token) )
    return boolean(to_b(token));

  if ( ischar(token) )
    return character(to_char(token));

  if ( !token || !*token )
    return nil();

  // probably a function called "+" or something
  return symbol(token, env);
}

static cons_t* parse_quote(environment_t* e);
static cons_t* parse_quasiquote(environment_t* e);
static cons_t* parse_unquote(environment_t* e);
static cons_t* parse_unquote_splicing(environment_t* e);
static cons_t* parse_vector(environment_t* e);

static long int parens = 0;

static bool isdot(const char* s)
{
  return s[0] == '.' && s[1] == '\0';
}

/*
 * TODO: Get rid of append() here.  It's extremely slow.
 *       See evlis() for hints.
 */
static cons_t* parse_list(environment_t *env, bool quoting = false)
{
  bool prev_dot = false;
  bool performed_cdr_dot = false;

  cons_t *p = NULL;
  const char *t;

  while ( (t = get_token()) != NULL && *t != ')' ) {
    /*
     * Detect all tokens that require a matching closing
     * parenthesis, such as "(" and "#(", etc.
     */
    bool paren = (*t == '(' || isvector(t) );

    // Track matching of parens
    if ( paren )
      ++parens;

    cons_t *add = NULL;

    if ( isdot(t) ) {
      prev_dot = true;
      continue;
    } else if ( issinglequote(t) )
      add = parse_quote(env);
    else if ( isquasiquote(t) )
      add = parse_quasiquote(env);
    else if ( isunquote(t) )
      add = parse_unquote(env);
    else if ( isunquote_splicing(t) )
      add = parse_unquote_splicing(env);
    else if ( isvector(t) )
      add = parse_vector(env);
    else
      add = paren? parse_list(env) : type_convert(t, env);

    if ( !prev_dot )
      p = nullp(p)? cons(add) : append(p, cons(add));
    else {
      if ( performed_cdr_dot )
        raise(runtime_exception(format(
          "Parser error: Invalid use of dot on '%s'", t)));

      { /*
         * Find end of p and set its cdr.
         */
        cons_t *e = p;
        while ( !nullp(cdr(e)) ) e = cdr(e);
        e->cdr = add;
      }

      performed_cdr_dot = true;
    }

    prev_dot = false;

    /* Added this to prevent the rest of the program to be
     * treated as being quoted. I.e., the following happened
     * before this:
     *
     * mickey> '(1 2 3) 4
     * translated to "(quote (1 2 3) 4)", which is wrong
     */
    if ( quoting )
      return p;//break;
  }

  if ( t && *t==')' )
    --parens;

  return p;
}

static cons_t* parse_quote(environment_t* e)
{
  /*
   * Replace '<expr> with (quote <expr>)
   */
  cons_t *expr = parse_list(e, true);

  /*
   * Special handling of the empty list, or '().
   */
  if ( nullp(expr) )
    return cons(symbol("list", e));

  return cons(symbol("quote", e), expr);
}

static cons_t* parse_vector(environment_t* e)
{
  /*
   * Replace #(<expr>) with (vector <expr>)
   */
  return cons(symbol("vector", e),
              parse_list(e, false));
}

static cons_t* parse_quasiquote(environment_t* e)
{
  /*
   * Replace `<expr> with (quasiquote <expr>)
   */
  cons_t *expr = parse_list(e, true);

  /*
   * Special handling of the empty list, or `()
   */
  if ( nullp(expr) )
    return cons(symbol("list", e));

  return cons(symbol("quasiquote", e), expr);
}

static cons_t* parse_unquote(environment_t* e)
{
  /*
   * Replace ,<expr> with (unquote <expr>)
   *
   * TODO: It is an error to perform this outside of
   *       quasiquotation.
   */
  return cons(symbol("unquote", e),
              parse_list(e, true));
}

static cons_t* parse_unquote_splicing(environment_t* e)
{
  /*
   * Replace ,@<expr> with (unquote-splicing <expr>)
   *
   * TODO: It is an error to perform this outside of
   *       quasiquotation.
   */
  return cons(symbol("unquote-splicing", e),
              parse_list(e, true));
}

program_t* parse(const std::string& program, environment_t *env)
{
  set_source(program.c_str());

  if ( env == NULL )
    raise(runtime_exception("parse: null environment"));

  program_t *p = new program_t();
  p->globals = env;
  p->parens = parens = 0;
  p->root = parse_list(p->globals);
  p->parens = parens;

  return p;
}
