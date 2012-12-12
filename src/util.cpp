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
#include <libgen.h>
#include <ctype.h>
#include <stdarg.h>
#include "util.h"
#include "primitives.h"
#include "parser.h"
#include "platform-limits.h"
#include "print.h"

std::string toupper(const std::string& str)
{
  const char* s = str.c_str();
  std::string r;
  while ( *s ) r += toupper(*s++);
  return r;
}

std::string tolower(const std::string& str)
{
  const char* s = str.c_str();
  std::string r;
  while ( *s ) r += tolower(*s++);
  return r;
}

char* trimr(char* s)
{
  size_t l = strlen(s);

  while ( l!=0 && isspace(s[l-1]) )
    s[--l] = '\0';

  return s;
}

int empty(const char* s)
{
  return s==NULL || *s=='\0';
}

bool char_in(char ch, const char* s)
{
  while ( *s )
    if ( ch == *s++ )
      return true;

  return false;
}

char* copy_str(const char* s)
{
  s = s? s : "";
#ifdef BOHEM_GC
  return strcpy((char*) GC_MALLOC(1 + strlen(s)), s);
#else
  return strcpy((char*) malloc(1 + strlen(s)), s);
#endif
}

std::string encode_str(const char* s)
{
  // TODO: Make this table-based or something
  std::string r;

  for ( ; *s; ++s ) {
    switch ( *s ) {
    default:
      if ( isprint(*s) )
        r += *s; // printable, add as-is
      else {
        unsigned char v = static_cast<unsigned char>(*s);
        r += format("\\x%.*x;", sizeof(v)*8/4, v);
      }
      break;
    case '\n': r += "\\n"; break;
    case '\r': r += "\\r"; break;
    case '\t': r += "\\t"; break;
    case '\\': r += "\\"; break;
    }
  }

  return r;
}

// indefinite article
std::string indef_art(const std::string& s)
{
  return (isvowel(s[0])? "an " : "a ") + s;
}

cons_t* deep_copy(const cons_t *p)
{
  if ( !p )
    return NULL;

  cons_t *r = new cons_t();
  memcpy(r, p, sizeof(cons_t));

  if ( listp(r) ) {
    r->car = deep_copy(r->car);
    r->cdr = deep_copy(r->cdr);
  } else if ( syntaxp(r) )
    r->syntax->transformer = deep_copy(r->syntax->transformer);
  else if ( stringp(r) )
    r->string = copy_str(r->string);

  return r;
}

std::string sbasename(const std::string& s)
{
  std::string r;
  char *p = strdup(s.c_str());
  r = basename(p);
  free(p);
  return r;
}

integer_t& min(integer_t& a, integer_t& b)
{
  return a<b? a : b;
}
