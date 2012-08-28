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

#include "mickey.h"

/*
 * Avoid name mangling, so we can actually
 * find back our functions in the scheme code
 * that implements this library.
 */
extern "C" {

cons_t* proc_backtrace(cons_t*, environment_t*)
{
  backtrace();
  return nil();
}

cons_t* proc_circularp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PAIR, car(p));
  return boolean(circularp(car(p)));
}

cons_t* proc_closure_source(cons_t* p, environment_t* e)
{
  assert_type(CLOSURE, car(p));
  closure_t *c = car(p)->closure;
  return cons(symbol("lambda", e), list(c->args, c->body));
}

cons_t* proc_debug(cons_t *p, environment_t *env)
{
  std::string s;
  s = format("%-11p type=%-7s", p, to_s(type_of(p)).c_str());

  switch ( type_of(p) ) {
  case NIL: break;
  case CHAR:
    s += format(" value=%d", p->character);
    break;
  case BOOLEAN:
    s += format(" value=%s", p->integer? "#t" : "#f");
    break;
  case DECIMAL:
    s += format(" value=%f", p->decimal);
    break;
  case INTEGER:
    s += format(" value=%d", p->integer);
    break;
  case SYNTAX:
    s += format(" syntax_transformer->%p environment->%p",
           p->syntax->transformer,
           p->syntax->environment);
    break;
  case CLOSURE:
    s += format(" function->%p environment->%p",
           p->closure->function,
           p->closure->environment);
    break;
  case PAIR:
    s += format(" car->%p cdr->%p", p->car, p->cdr);
    break;
  case SYMBOL:
    s += format(" name='%s'", p->symbol->name().c_str());
    break;
  case STRING:
    s += format(" value='%s'", p->string);
    break;
  case VECTOR:
    s += format(" vector->%p", p->vector);
    break;
  case BYTEVECTOR:
    s += format(" bytevector->%p", p->bytevector);
    break;
  case PORT:
    s += format(" port->%p", p->port);
    break;
  case ENVIRONMENT:
    s += format(" environment->%p", p->environment);
    break;
  case POINTER:
    s += format(" pointer->%p", p->pointer);
    break;
  case CONTINUATION:
    break;
  }

  s += "\n";

  if ( type_of(p) == PAIR ) {
    s += proc_debug(car(p), env)->string;
    s += proc_debug(cdr(p), env)->string;
  }

  return string(s.c_str());
}

/*
 * Create cons graph for given list that can be rendered by Graphviz.
 *
 * Example usage:
 *
 * /mickey -e '(display (:list->dot (quote (define (square x) (* x x * 123)))))' | dot -Tpng -o graph.png && open graph.png 
 *
 */
cons_t* proc_list_to_dot_helper(cons_t *p, environment_t* e)
{
  static const char* line_style = "[\"ol\"=\"box\"]";
  static const char* shape = "record";

  if ( nullp(p) ) return string("");

  std::string s;

  if ( pairp(p) ) {
    if ( !nullp(car(p)) ) {
      const char* port = "";
      if ( pairp(car(p)) ) port = ":head";
      s += format("  \"%p\":head -> \"%p\"%s %s;\n", p, car(p), port, line_style);
      s += proc_list_to_dot_helper(car(p), e)->string;
    }
    if ( !nullp(cdr(p)) ) {
      const char* port = "";
      if ( pairp(cdr(p)) ) port = ":head";
      s += format("  \"%p\":tail -> \"%p\"%s %s;\n", p, cdr(p), port, line_style);
      s += proc_list_to_dot_helper(cdr(p), e)->string;
    }
    s += format("  \"%p\" [label=\"<head>|<tail>\", shape=\"%s\"];\n", p, shape);
  } else
    s += format("  \"%p\" [label=\"%s\", shape=\"none\"];\n",
                p, sprint(p).c_str());

  return string(s.c_str());
}

cons_t* proc_list_to_dot(cons_t *p, environment_t* e)
{
  std::string s = "digraph Scheme {\n";
  s += proc_list_to_dot_helper(p, e)->string;
  s += "}\n";
  return string(s.c_str());
}

/*
 * Usage: (define-syntax foo ...) ... (:syntax-expand '(foo arg0 arg1 ...))
 */
cons_t* proc_syntax_expand(cons_t* p, environment_t *e)
{
  cons_t *code = car(p);
  assert_type(PAIR, code);

  cons_t *syntax_name = car(code);
  assert_type(SYMBOL, syntax_name);
  cons_t *op = e->lookup_or_throw(syntax_name->symbol->name());

  assert_type(SYNTAX, op);
  return syntax_expand(op, code, e);
}

cons_t* proc_version(cons_t*, environment_t*)
{
  return list(
    string(VERSION),
    string(format("Compiler version: %s", __VERSION__).c_str()));
}

cons_t* proc_type_of(cons_t* p, environment_t* e)
{
  return symbol(to_s(type_of(car(p))).c_str(), e);
}

named_function_t exports_mickey_misc[] = {
  {":backtrace", proc_backtrace, false},
  {":circular?", proc_circularp, false},
  {":closure-source", proc_closure_source, false},
  {":debug", proc_debug, false},
  {":list->dot", proc_list_to_dot, false},
  {":syntax-expand", proc_syntax_expand, false},
  {":type-of", proc_type_of, false},
  {":version", proc_version, false},
  {NULL, NULL, false}
};

}; // extern "C"
