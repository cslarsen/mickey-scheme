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

#include <ctype.h>
#include "exceptions.h"
#include "parser.h"
#include "print.h"
#include "util.h"

int count(const char *s, int (*check)(int))
{
  int n = 0;
  while ( !empty(s) )
    if ( check(*s++) )
      ++n;
  return n;
}

int all(const char* s, int (*check)(int))
{
  if ( empty(s) )
    return false;

  while ( !empty(s) )
    if ( !check(*s++) )
      return false;
  return true;
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
