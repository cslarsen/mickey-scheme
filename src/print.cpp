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

#include "print.h"
#include "util.h"
#include "primitives.h"

std::string sprint(const cons_t* p, std::string& s, bool escape)
{
  // special handling of the empty list
  //if ( emptylistp(p) )
  //  return s + "()";

  switch ( type_of(p) ) {
  case NIL:          return s;
  case BOOLEAN:      return s + to_s(p->boolean);
  case CHAR:         return s + to_s(p->character, escape);
  case REAL:         return s + to_s(p->real);
  case INTEGER:      return s + to_s(p->integer);
  case RATIONAL:     return s + to_s(p->rational);
  case CLOSURE:      return s + (escape? to_s(p->closure) : "");
  case SYMBOL:       return s + *p->symbol;
  case STRING:       return s + (escape? "\"" + encode_str(p->string) + "\"" : p->string);
  case VECTOR:       return s + sprint(p->vector, s, escape);
  case BYTEVECTOR:   return s + sprint(p->bytevector, s, escape);
  case CONTINUATION: return s + (escape? to_s(p->continuation) : "");
  case SYNTAX:       return s + sprint(p->syntax->transformer, s, escape);
  case PORT:         return s + sprint(p->port, s, escape);
  case ENVIRONMENT:  return s + sprint(p->environment, s, escape);
  case POINTER:      return s + sprint(p->pointer, s, escape);
  case PAIR: {
    std::string head = sprint(car(p), s, escape);
    std::string tail = (atomp(cdr(p)) && !nullp(cdr(p)) ?
                        ". " : "") + sprint(cdr(p), s, escape);
    return s
      + (type_of(car(p))==PAIR ? "(" : "")
      + head
      + (type_of(car(p))==PAIR ? ")" : "")
      + (!tail.empty() ? " " : "") + tail;
  }}

  return s;
}

std::string sprint(const cons_t* p)
{
  std::string s;
  return sprint(pairp(p) ? cons(p) : p, s, true);
}

std::string print(const cons_t* p)
{
  std::string s;
  return sprint(list(p) ? cons(p) : p, s, false);
}

std::string sprint(const vector_t* v, std::string& r, bool)
{
  const std::vector<cons_t*>& p = v->vector;
  std::string s;
  s += "#(";

  for ( std::vector<cons_t*>::const_iterator i = p.begin();
        i != p.end(); ++i )
  {
    if ( i != p.begin() ) s += " ";
    if ( listp(*i) ) s += "(";

    if ( nullp(*i) )
      s += to_s(*i);
    else
      s += sprint(*i, r, true);

    if ( listp(*i) ) s += ")";
  }

  s += ")";
  return s;
}

std::string sprint(const bytevector_t* v, std::string&, bool)
{
  const std::vector<uint8_t>& p = v->bytevector;
  std::string s;
  s += "#u8(";

  std::string space = "";

  for ( std::vector<uint8_t>::const_iterator i = p.begin();
        i != p.end(); ++i )
  {
    s += space + to_s(*i);
    space = " ";
  }

  s += ")";
  return s;
}

std::string sprint(const port_t* p, std::string&, bool)
{
  return format("#<port %p %s%s%s%s>", p, p->readable? "R" : "", p->writable? "W" : "",
      p->istextual()? "T" : "", p->isbinary()? "B" : "");
}

std::string sprint(const environment_t* p, std::string&, bool)
{
  return format("#<environment %p>", p);
}

std::string sprint(const pointer_t* p, std::string&, bool)
{
  return format("#<pointer '%s' %p>", p->tag, p->value);
}

std::string to_s(integer_t n)
{
  char buf[64];
  sprintf(buf, "%d", n);
  return std::string(buf);
}

std::string to_s(rational_t n)
{
  char buf[64];
  sprintf(buf, "%d/%d", n.numerator, n.denominator);
  return std::string(buf);
}

std::string to_s(real_t n)
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

std::string to_s(enum type_t type)
{
  switch ( type ) {
  case NIL:          return "nil";          break;
  case BOOLEAN:      return "boolean";      break;
  case CHAR:         return "char";         break;
  case REAL:         return "real";         break;
  case INTEGER:      return "integer";      break;
  case RATIONAL:     return "rational";     break;
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
  case REAL:     return to_s(p->real);
  case INTEGER:  return to_s(p->integer);
  case RATIONAL: return to_s(p->rational);
  case CLOSURE:  return format("#<closure %p>", p->closure);
  case PAIR:     return to_s(car(p)) + " . " + to_s(cdr(p));
  case SYMBOL:   return *p->symbol;
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

