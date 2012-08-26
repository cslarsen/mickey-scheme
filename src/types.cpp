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

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "types.h"
#include "util.h"
#include "exceptions.h"

static int count(const char *s, int (*check)(int))
{
  int n = 0;
  while ( !empty(s) )
    if ( check(*s++) )
      ++n;
  return n;
}

static int all(const char* s, int (*check)(int))
{
  if ( empty(s) )
    return false;

  while ( !empty(s) )
    if ( !check(*s++) )
      return false;
  return true;
}

static int isdot(int s)
{
  return s == '.';
}

static int ishex(int s)
{
  return isdigit(s) ||
    (s>='a' && s<='f') ||
    (s>='A' && s<='F');
}

bool ishex(const char* s)
{
  return !empty(s) && all(s, ishex);
}

bool isinteger(const char* s)
{
  int sign = (s[0]=='-' || s[0]=='+');
  return !empty(s) && all(s+sign, isdigit);
}

bool isbool(const char* s)
{
  return s[0]=='#' && (s[1]=='t' || s[1]=='f');
}

bool ischar(const char* s)
{
  return s[0]=='#' && s[1]=='\\';
}

bool isfloat(const char* s)
{
  // TODO: Make pattern complete
  // Pattern now: [+-]?[0-9]*\.?[0-9]+f?
  size_t dots   = count(s, isdot);
  size_t sign   = (s[0]=='-' || s[0]=='+');
  size_t digits = count(s, isdigit);
  size_t last_f = (s[0]? s[strlen(s)-1] == 'f' : 0);

  /*
   * The parts of a good looking floating point
   * number are: [+-] [0-9] [.] [0-9] [f]
   *
   * Now, the number of digits + MAX ONE dot
   * + MAX ONE trailing 'f' should equal the
   * length of the string -- for a finely formatted
   * floating point number.
   *
   * I know this is a stretch, but I'm NOT going
   * to the pain of installing a regex just to parse
   * a frickin number :-)
   *
   */

  if ( strlen(s+sign) == (digits + (dots==1) + last_f) )
    return digits>0 && (dots==1 || last_f); // or else it's an ordinary integer
  else
    return false;
}

bool isodd(int n)
{
  return n & 1;
}

bool isstring(const char* s)
{
  size_t l = strlen(s);

  // a string must start and end with quote: "..."
  if ( s==NULL || l<=1 || s[0]!='"' || s[l-1]!='"' ) {

    /*
     * If token begins with a doublequote, then we've
     * got a malformed string.
     */
    if ( s[0]=='"' )
      raise(compiler_exception("Not a valid string: " + std::string(s)));

    return false;
  }

  // count quotes
  size_t quotes = 0;

  for ( const char *p=s; p < s+l; ++p ) {
    switch ( *p ) {
      case '\\':
        // skip escaped character
        ++p;
        continue;
      case '"':
        ++quotes;

        // when we've hit ending quote, check that
        // we're at the end-position
        if ( quotes == 2 )
          return strlen(p+1) == 0;

        break;
      default:
        continue;
        break;
    }
  }

  // should've finished at second quote over, so must be error
  return false;
}

bool isatom(const char* s)
{
  return isalpha(s[0]) && (empty(s+1) ? true : all(s+1, isalnum));
}

bool isvector(const char* s)
{
  return s[0]=='#' && s[1]=='(';
}

decimal_t to_f(const char* s)
{
  return static_cast<decimal_t>(atof(s));
}

int to_i(const char* s)
{
  if ( s == NULL )
    raise(runtime_exception("Cannot convert NULL to INTEGER"));

  int has_sign = (char_in(*s, "+-"));
  int sign = (s[0]=='-'? -1 : 1);

  return sign * atoi(has_sign + s);
}

bool to_b(const char* s)
{
  return s[1]=='t'? true : false;
}

char to_char(const char* s)
{
  // Example: "#\ "
  // TODO: How will this work? `(display #\<CTRL+V, CTRL+TAB>)´ ?
  if ( strlen(s) == 2 )
    return ' ';

  // Example: "#\A", etc.
  if ( strlen(s) == 3 )
    // TODO: Only allow SPECIFIC character ranges
    return s[2];

  // Example: "#\x28" -> '('
  if ( strlen(s)>3 && s[2] == 'x' && ishex(s+3) ) {
    long code = strtol(s+3, (char**)NULL, 16);

    if ( code > 0x7F ) // U+0000 --- U+007F, i.e., the ASCII repertoire
      raise(runtime_exception("Unicode characters are not suppoted"));

    return static_cast<char>(code);
  }

  // Example: "#\space"
  // TODO: Make ONE giant table that we use for all character mappings
  s += 2;
  std::string lit = tolower(s);

  if ( lit == "space" )     return ' ';
  if ( lit == "tab" )       return '\t';
  if ( lit == "newline" )   return '\n';
  if ( lit == "return"  )   return '\r';
  if ( lit == "null"  )     return '\0';
  if ( lit == "alarm" )     return '\a';
  if ( lit == "backspace" ) return '\x8';
  if ( lit == "escape" )    return '\x1b';
  if ( lit == "delete" )    return '\x7f';

  raise(runtime_exception(format("Unrecognized character literal: #\\%s", s)));
  return '\0'; // make compiler happy
}

bool isvowel(char c)
{
  switch ( c ) {
  case 'a': case 'e': case 'i':
  case 'o': case 'u': case 'y':
    return true;
  default:
    return false;
  }
}

bool issinglequote(const char* s)
{
  return *s=='\'';
}

bool isquasiquote(const char* s)
{
  return *s == '`';
}

bool isunquote(const char* s)
{
  return *s == ',' && *(s+1) != '@';
}

bool isunquote_splicing(const char *s)
{
  return *s == ',' && *(s+1) == '@';
}

enum type_t to_type_t(const char* s)
{
  switch ( s[0] ) {
  default:
  case 'n': return NIL;
  case 'b': return !strcmp(s, "boolean")? BOOLEAN : NIL;
  case 'c': return !strcmp(s, "char")? CHAR :
                   !strcmp(s, "closure")? CLOSURE :
                   !strcmp(s, "continuation")? CONTINUATION : NIL;
  case 'i': return !strcmp(s, "integer")? INTEGER : NIL;
  case 'd': return !strcmp(s, "decimal")? DECIMAL : NIL;
  case 'p': return !strcmp(s, "pair")? PAIR : NIL;
  case 's': return !strcmp(s, "symbol")? SYMBOL : !strcmp(s, "string")? STRING : NIL;
  case 'v': return !strcmp(s, "vector")? VECTOR : NIL;
  }
}
