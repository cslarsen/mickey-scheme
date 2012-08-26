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

#include <libgen.h>
#include <ctype.h>
#include <stdarg.h>
#include "util.h"
#include "primitives.h"
#include "types.h"
#include "platform-limits.h"

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

std::string to_s(int n)
{
  char buf[64];
  sprintf(buf, "%d", n);
  return std::string(buf);
}

std::string to_s(decimal_t n)
{
  char buf[64];
  sprintf(buf, "%g", n);
  return std::string(buf);
}

std::string to_s(bool f)
{
  return std::string(f? "#t" : "#f");
}

std::string format(const char* fmt, ...)
{
  char buf[1024] = {'\0'};
  va_list list;
  va_start(list, fmt);
  vsprintf(buf, fmt, list);
  va_end(list);
  return std::string(buf);
}

int empty(const char* s)
{
  return s==NULL || *s=='\0';
}

const char* skip_space(const char* s)
{
  while ( isspace(*s) ) ++s;
  return s;
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

std::string to_s(enum type_t type)
{
  switch ( type ) {
  case NIL:          return "nil";          break;
  case BOOLEAN:      return "boolean";      break;
  case CHAR:         return "char";         break;
  case DECIMAL:      return "decimal";      break;
  case INTEGER:      return "integer";      break;
  case CLOSURE:      return "closure";      break;
  case PAIR:         return "pair";         break;
  case SYMBOL:       return "symbol";       break;
  case SYNTAX:       return "syntax";       break;
  case STRING:       return "string";       break;
  case VECTOR:       return "vector";       break;
  case CONTINUATION: return "continuation"; break;
  case BYTEVECTOR:   return "bytevector";   break;
  case PORT:         return "port";         break;
  case ENVIRONMENT:  return "environment";  break;
  case POINTER:      return "pointer";      break;
  }

  return "#<unknown type>";
}

std::string to_s(cons_t *p)
{
  switch ( type_of(p) ) {
  case NIL:      return "#<nil>";
  case BOOLEAN:  return to_s(p->boolean);
  case CHAR:     return to_s(p->character, false);
  case DECIMAL:  return to_s(p->decimal);
  case INTEGER:  return to_s(p->integer);
  case CLOSURE:  return format("#<closure %p>", p->closure);
  case PAIR:     return to_s(car(p)) + " . " + to_s(cdr(p));
  case SYMBOL:   return p->symbol->name();
  case SYNTAX:   return format("#<syntax_transformer %p>", p->syntax);
  case STRING:   return p->string;
  case VECTOR:   return format("#<vector %p>", p->vector);
  case PORT:     return format("#<port %p>", p->port);
  case CONTINUATION: return format("#<continuation %p>", p->continuation);
  case BYTEVECTOR:   return format("#<bytevector %p>", p->bytevector);
  case ENVIRONMENT:  return format("#<environment %p>", p->environment);
  case POINTER:      return format("#<pointer '%s' %p>",
                              p->pointer->tag, p->pointer->value);
  }

  return "#<unknown type>";
}

std::string to_s(closure_t* p)
{
  return format("#<closure %p>", p);
}

std::string to_s(continuation_t* p)
{
  return format("#<continuation %p>", p);
}

std::string to_s(vector_t* p)
{
  return format("#<vector %p>", p);
}

std::string to_s(port_t* p)
{
  return format("#<port %p>", p);
}

std::string to_s(char p, bool escape)
{
  return format(escape? "#\\%c" : "%c", isprint(p)? p : '?' );
}

std::string to_s(environment_t* e)
{
  return format("#<environment %p", e);
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
