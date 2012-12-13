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

#include "strings.h"
#include "exceptions.h"
#include "parser.h"
#include "primitives.h"
#include "print.h"
#include "util.h"
#include "types/radix_t.h"
#include "debug.h"

static cons_t* parse_bytevector(environment_t* e);
static cons_t* parse_quasiquote(environment_t* e);
static cons_t* parse_quote(environment_t* e);
static cons_t* parse_token(const char* tok, bool paren, environment_t* env);
static cons_t* parse_unquote(const char* t, bool paren, environment_t* e);
static cons_t* parse_unquote_splicing(environment_t* e);
static cons_t* parse_vector(environment_t* e);
static long int parens = 0;

static bool hasradix(const char* s)
{
  if ( s[0] != '#' )
    return false;

  switch ( tolower(s[1]) ) {
  case 'b':
  case 'o':
  case 'd':
  case 'x':
    return true;

  default:
    return false;
  }
}

static bool hasexactprefix(const char* s)
{
  if ( s[0] != '#' )
    return false;

  switch ( tolower(s[1]) ) {
  case 'e':
  case 'i':
    return true;

  default:
    return false;
  }
}

static radix_t parse_radix(const char* s)
{
  if ( s[0] == '#' )
  switch ( tolower(s[1]) ) {
  case 'b': return BINARY;
  case 'o': return OCTAL;
  case 'd': return DECIMAL;
  case 'x': return HEXADECIMAL;
  default : break;
  }

  raise(parser_exception(format(
    "Invalid radix specifier on line %d: %s",
      current_line_number(), s)));

  return BINARY; // to make compiler happy
}

static bool parse_exact_prefix(const char* s)
{
  if ( s[0] == '#' )
  switch ( tolower(s[1]) ) {
  case 'e': return true;
  case 'i': return false;
  default:  break;
  }

  raise(parser_exception(format(
    "Invalid exactness prefix on line %d: %s",
      current_line_number(), s)));

  return false; // to make compiler happy
}

cons_t* type_convert(const char* token)
{
  const char* token_start = token;

  bool has_exact_prefix = false;
  bool has_radix_prefix = false;

  bool is_exact_number = false;
  radix_t radix = DECIMAL;

  if ( hasradix(token) ) {
    radix = parse_radix(token);
    has_radix_prefix = true;
    token += 2;
  }

  if ( hasexactprefix(token) ) {
    is_exact_number = parse_exact_prefix(token);
    has_exact_prefix = true;
    token += 2;
  }

  /*
   * See, this is a trick.  The report says that the radix and exactness
   * prefices can come in any order, so therefore we check again here to
   * allow for that.
   */
  if ( hasradix(token) ) {
    radix = parse_radix(token);
    has_radix_prefix = true;
    token += 2;
  }

  /*
   * If we found radix or exactness prefix above, we MUST have a number now.
   */

  if ( isreal(token) ) {
    if ( !is_exact_number )
      return real(to_f(token, radix));
    return parse_exact_real(token, radix);
  }

  if ( isrational(token) )
    return rational(to_r(token, radix),
                   has_exact_prefix? is_exact_number : true);

  if ( isinteger(token, radix) )
    return integer(to_i(token, radix),
                   has_exact_prefix? is_exact_number : true);

  /*
   * TODO: When we've got complex numbers, this is the spot to handle them.
   */

  /*
   * If we have radix/exactness prefix, we should have a number, and so this
   * should be an error.
   */
  if ( has_radix_prefix || has_exact_prefix )
    raise(parser_exception(format(
      "Invalid use of prefix with non-number on line %d: %s",
        current_line_number(), token_start)));

  if ( isstring(token) )
    return parse_string(token);

  if ( isatom(token) )
    return symbol(!fold_case()? token : string_foldcase(token).c_str());

  if ( isbool(token) )
    return boolean(to_b(token));

  if ( ischar(token) )
    return character(to_char(token));

  if ( !token || !*token )
    return nil();

  // probably a function called "+" or something
  return symbol(!fold_case()? token : string_foldcase(token).c_str());
}

static bool isdot(const char* s)
{
  return s[0] == '.' && s[1] == '\0';
}

static bool isparen(const char* s)
{
  return *s == '(' || isvector(s) || isbytevector(s);
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
     * parenthesis, such as "(" and "#(", "#u8("etc.
     */
    bool paren = isparen(t);

    // Track matching of parens
    if ( paren )
      ++parens;

    if ( isdot(t) ) {
      prev_dot = true;
      continue;
    }

    cons_t *add = parse_token(t, paren, env);

    if ( !prev_dot )
      p = nullp(p)? cons(add) : append(p, cons(add));
    else {
      if ( performed_cdr_dot )
        raise(runtime_exception(format(
          "Parser error on line %d: Invalid use of dot on '%s'",
            current_line_number(), t)));

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
      return p;
  }

  if ( t && *t==')' )
    --parens;

  return p;
}

static cons_t* parse_token(const char* t, bool paren, environment_t* env)
{
  cons_t* add = NULL;

  if ( issinglequote(t) )
    add = parse_quote(env);
  else if ( isquasiquote(t) )
    add = parse_quasiquote(env);
  else if ( isunquote(t) ) {
    add = parse_unquote(t, paren, env);
  } else if ( isunquote_splicing(t) )
    add = parse_unquote_splicing(env);
  else if ( isvector(t) )
    add = parse_vector(env);
  else if ( isbytevector(t) )
    add = parse_bytevector(env);
  else
    add = paren? parse_list(env) : type_convert(t);

  return add;
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
    return cons(symbol("list"));

  return cons(symbol("quote"), expr);
}

static cons_t* parse_unquote(const char* t, bool paren, environment_t* e)
{
  /*
   * Replace ,<expr> with (unquote <expr>)
   *
   * TODO: It is an error to perform this outside of
   *       quasiquotation.
   */
  cons_t* s = *(t+1)!='\0'?
    parse_token(t+1, paren, e) : /* form ",<token>" */
    car(parse_list(e, true)); /* form ",(<token (s)>)" */

  if ( nullp(s) )
    raise(runtime_exception(format(
      "Parser error on line %d: Empty (unquote) form",
        current_line_number())));

  return cons(symbol("unquote"), cons(s));
}

static cons_t* parse_vector(environment_t* e)
{
  /*
   * Replace #(<expr>) with (vector <expr>)
   */
  return cons(symbol("vector"), parse_list(e, false));
}

static cons_t* parse_bytevector(environment_t* e)
{
  /*
   * Replace #u8(<expr>) with (vector <expr>)
   */
  return cons(symbol("bytevector"), parse_list(e, false));
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
    return cons(symbol("list"));

  return cons(symbol("quasiquote"), expr);
}

static cons_t* parse_unquote_splicing(environment_t* e)
{
  /*
   * Replace ,@<expr> with (unquote-splicing <expr>)
   *
   * TODO: It is an error to perform this outside of
   *       quasiquotation.
   */
  return cons(symbol("unquote-splicing"), parse_list(e, true));
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
