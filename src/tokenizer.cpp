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
#include <stdlib.h> // NULL
#include <string.h>
#include <ctype.h> // isspace, et al
#include "tokenizer.h"
#include "util.h"

static const char* source = NULL;
static bool inside_string = false;

void set_source(const char* program)
{
  source = program;
  inside_string = false;
}

static bool string_or_non_delimiter(const char* s)
{
  char ch = *s;
  bool open_paren = (ch=='(' /* normal paren, or ... */
      || (s[0]=='#' && s[1]=='(')); /* vector form #( ... ) */

  if ( ch == '\"' )
    inside_string = !inside_string;

  return ch!='\0'
    && (inside_string? true :
          !open_paren && ch!=')' && !isspace(ch));
}

static const char* copy_while(
    char *dest, const char* src, bool (*while_expr)(const char*))
{
  while ( while_expr(src) )
    *dest++ = *src++;

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
      while ( *source != '\n' ) ++source;
      continue;
    }

    // hash-bang or similar? skip to end of line
    // TODO: Properly handle reader directives like case-folding, etc.
    if ( source[0]=='#' && source[1]=='!' ) {
      while ( *source != '\n' ) ++source;
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

    if ( char_in(*source, "()'") )
      // tokens ( and )
      token[0] = *source++;
    else
      // other tokens
      source = copy_while(token, source, string_or_non_delimiter);

    // commented datums "#;"
    if ( token[0]=='#' && token[1]==';' ) {

      // token begins with "#;", so skip it
      if ( token[2] != '\0' )
        continue;

      // skip current token AND next, then continue
      get_token();
      continue;
    }

    // emit NULL when finished
    return !empty(token) ? token : NULL;
  }
}
