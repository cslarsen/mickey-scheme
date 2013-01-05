/*
 * Mickey R7RS Scheme
 *
 * Copyright (C) 2011-2012 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "exceptions.h"
#include "print.h"
#include "tokenizer.h"
#include "util.h"

static const char* source_start = NULL;
static const char* source = NULL;
static bool inside_string = false;
static bool fold_case_flag = false;
static int line = 1;

void set_fold_case(bool setting)
{
  fold_case_flag = setting;
}

static bool hasprefix(const char* str, const char* prefix)
{
  size_t len = strlen(prefix);

  while ( len-- )
    if ( *str++ != *prefix++ )
      return false;

  return true;
}

int current_line_number()
{
  return line;
}

static inline void checkline(const char ch)
{
  if ( ch == '\n' )
    ++line;
}

bool get_fold_case()
{
  return fold_case_flag;
}

void set_source(const char* program)
{
  source_start = source = program;
  inside_string = false;
  line = 1;
}

static bool not_pipe(const char* s)
{
  return *s!='|';
}

static bool string_or_non_delimiter(const char* s)
{
  static bool escape_char_signalled = false; // e.g. '\\' and then '\"'
  static bool stop_signalled = false;

  if ( stop_signalled ) {
    stop_signalled = false;
    return false;
  }

  char ch = *s;

  /*
   * If we have been told to parse an escaped character, then parse it as
   * long as the character is not null or space.
   */
  if ( escape_char_signalled && !isspace(ch) && ch!='\0' ) {
    escape_char_signalled = false;
    return true;
  }

  // ignore next datum symbol #; is a token
  if ( s[0]=='#' && s[1]==';' )
    return false;

  /*
   * Check for:
   *
   *   (1) Normal list form, (...), and NOT #\( character literal
   *   (2) Vector form, #(...)
   *   (3) Bytevector form, #u8(...)
   *   (4) Closing of normal list form but NOT a #\) character literal
   *
   */
  bool char_literal  = (source - source_start) > 2 && hasprefix(s-2, "#\\");
  bool open_list     = ch=='(' && !char_literal;  // (1)
  bool open_vector   = hasprefix(s, "#(");        // (2)
  bool open_u8vector = hasprefix(s, "#u8(");      // (3)
  bool close_list    = ch==')' && !char_literal;  // (4)

  // Now determine if we should look for a closing paren
  bool require_close_list = open_list || open_vector || open_u8vector;

  /*
   * If we are parsing a string and the current character is backspace, then
   * we need to remember that for the next character, because it could be an
   * escaped character like "in \"this\" string".
   */
  if ( ch == '\\' && inside_string ) {
    escape_char_signalled = true;
    return true;
  }

  /*
   * If we find a " character there are three possible situations:
   *
   *   - We will start parsing a new string
   *   - We are parsing en escaped "-character inside a string
   *   - We are ending the parsing of a string
   */
  if ( ch == '\"' ) {
    if ( !escape_char_signalled ) {
      inside_string = !inside_string;
      if ( !inside_string ) // just parsed a string
        stop_signalled = true; // so signal token end
    }
  }

  return ch!='\0'
    && (inside_string? true :
          !require_close_list && !close_list && !isspace(ch));
}

static const char* skip_space(const char* s)
{
  while ( isspace(*s) ) {
    checkline(*s);
    ++s;
  }

  return s;
}

static const char* copy_while(
    char *dest, const char* src,
    size_t destsize,
    bool (*while_expr)(const char*))
{
  while ( *src!='\0' && destsize-- && while_expr(src) ) {
    checkline(*src);
    *dest++ = *src++;
  }

  *dest = '\0';
  return src;
}

const char* get_token()
{
  // mutatable return buffer
  static char token[256];

  for ( ;; ) {
    token[0] = token[1] = '\0';
    source = skip_space(source);

    // comment? skip to end of line
    if ( *source == ';' ) {
      while ( *source != '\n' ) {
        ++source;

        if ( *source == '\0' )
          return NULL;
      }

      continue;
    }

    // hash-bang or similar? skip to end of line
    // TODO: Properly handle reader directives like case-folding, etc.
    if ( source[0]=='#' && source[1]=='!' ) {

      // skip to end of line
      const char *start = source;
      while ( *source != '\n' ) ++source;

      if ( !strncmp("#!fold-case", start, source - start) )
        fold_case_flag = true;
      else if ( !strncmp("#!no-fold-case", start, source - start) )
        fold_case_flag = false;

      continue;
    }

    // block-comments?
    if ( source[0]=='#' && source[1]=='|' ) {
      // match nested pairs
      source += 2;
      for ( int n=1; n && *source; ++source ) {
             if ( source[0]=='#' && source[1]=='|' ) { ++source; ++n; }
        else if ( source[0]=='|' && source[1]=='#' ) { ++source; --n; }
      }
      continue;
    }

    // vector form "#( ... )"
    if ( source[0]=='#' && source[1]=='(' ) {
      strcpy(token, "#(");
      source += 2;
      return token;
    }

    // bytevector form "#u8( ... )"
    if ( source[0]=='#' && source[1]=='u' &&
         source[2]=='8' && source[3]=='(' )
    {
      strcpy(token, "#u8(");
      source += 4;
      return token;
    }

    // ignore-next-datum form "#;"
    if ( source[0]=='#' && source[1]==';' ) {
      strcpy(token, "#;");
      source += 2;
      return token;
    }

    if ( char_in(*source, "()'") )
      // tokens ( and )
      token[0] = *source++;
    else {
      // long-form-symbol w/format "|foo bar baz|"
      if ( source[0]=='|' ) {
        const size_t lineno = line;
        token[0]='|';
        source = copy_while(token+1, source+1, sizeof(token)-2, not_pipe);

        if ( *source == '|' )
          ++source;
        else
          raise(parser_exception(format(
            "Invalid |long symbol| on line %lu\n", lineno)));

        const size_t l = strlen(token);
        token[l] = '|';
        token[l+1] = '\0';
      } else
        // other tokens
        source = copy_while(token, source, sizeof(token)-1,
                            string_or_non_delimiter);
    }

    // emit NULL when finished
    return !empty(token) ? token : NULL;
  }
}
