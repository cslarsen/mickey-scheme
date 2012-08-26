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

std::string sprint(const vector_t*, std::string&, bool);
std::string sprint(const bytevector_t*, std::string&, bool);
std::string sprint(const port_t*, std::string&, bool);
std::string sprint(const environment_t*, std::string&, bool);
std::string sprint(const pointer_t*, std::string&, bool);

std::string sprint(const cons_t* p, std::string& s, bool escape)
{
  // special handling of the empty list
  //if ( emptylistp(p) )
  //  return s + "()";

  switch ( type_of(p) ) {
  case NIL:          return s;
  case BOOLEAN:      return s + to_s(p->boolean);
  case CHAR:         return s + to_s(p->character, escape);
  case DECIMAL:      return s + to_s(p->decimal);
  case INTEGER:      return s + to_s(p->integer);
  case CLOSURE:      return s + (escape? to_s(p->closure) : "");
  case SYMBOL:       return s + p->symbol->name();
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

std::string sprint(const program_t* p)
{
  return sprint(p->root);
}

std::string print(const cons_t* p)
{
  std::string s;
  return sprint(list(p) ? cons(p) : p, s, false);
}

std::string print(const program_t* p)
{
  return print(p->root);
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
