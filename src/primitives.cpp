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

#include <stdint.h> // limits
#include <math.h> // floor
#include "primitives.h"
#include "util.h"
#include "assertions.h"
#include "print.h"
#include "circular.h"
#include "exceptions.h"
#include "import.h"

/*
 * Marks that return-value is unspecified.
 */
cons_t* unspecified(cons_t* p)
{
  return p;
}

cons_t* cons(const cons_t* h, const cons_t* t)
{
  cons_t *p = new cons_t();
  p->type = PAIR;
  p->car = const_cast<cons_t*>(h);
  p->cdr = const_cast<cons_t*>(t);
  return p;
}

cons_t* list(const cons_t* h, const cons_t* t)
{
  return cons(h, t? cons(t) : NULL);
}

cons_t* symbol(const char* s)
{
  cons_t *p = new cons_t();
  p->type = SYMBOL;
  p->symbol = create_symbol(s);
  return p;
}

cons_t* nil()
{
  cons_t *p = new cons_t();
  p->type = NIL;
  return p;
}

cons_t* integer(integer_t n, bool exact)
{
  cons_t *p = new cons_t();
  p->type = INTEGER;
  p->integer = n;
  p->exact = exact;
  return p;
}

cons_t* rational(rational_t r, bool exact)
{
  if ( r.denominator == 0 )
    raise(runtime_exception("Cannot divide by zero: " + to_s(r)));

  cons_t *p = new cons_t();
  p->type = RATIONAL;
  p->rational = simplify(r);
  p->exact = exact;

  /*
   * Can we promote to an integer?
   */
  if ( p->rational.denominator == 1 ) {
    p->type = INTEGER;
    p->integer = r.numerator;
  }

  return p;
}

cons_t* port(port_t* p)
{
  cons_t *r = new cons_t();
  r->type = PORT;
  r->port = p;
  return r;
}

cons_t* environment(environment_t* e)
{
  cons_t *r = new cons_t();
  r->type = ENVIRONMENT;
  r->environment = e;
  return r;
}

cons_t* pointer(pointer_t* p)
{
  cons_t *r = new cons_t();
  r->type = POINTER;
  r->pointer = p;
  return r;
}

cons_t* pointer(const char* tag, void* value)
{
  cons_t *r = new cons_t();
  r->type = POINTER;
  r->pointer = new pointer_t(tag, value);
  return r;
}

cons_t* real(real_t n)
{
  cons_t *p = new cons_t();
  p->type = REAL;
  p->real = n;
  p->exact = false;
  return p;
}

cons_t* real(rational_t r)
{
  cons_t *p = new cons_t();
  p->type = REAL;
  p->exact = false;
  p->real = static_cast<real_t>(r.numerator) /
            static_cast<real_t>(r.denominator);
  return p;
}

cons_t* boolean(bool f)
{
  cons_t *p = new cons_t();
  p->type = BOOLEAN;
  p->boolean = f;
  return p;
}

cons_t* character(character_t c)
{
  cons_t *p = new cons_t();
  p->type = CHAR;
  p->character = c;
  return p;
}

cons_t* string(const char* s)
{
  cons_t *p = new cons_t();
  p->type = STRING;
  p->string = copy_str(s);
  return p;
}

cons_t* vector(cons_t* p, size_t size, cons_t* fill)
{
  vector_t *v;

  if ( size )
    v = fill? new vector_t(size, fill) : new vector_t(size, nil());
  else {
    v = new vector_t();

    while ( !nullp(p) ) {
      v->vector.push_back(car(p));
     p = cdr(p);
    }
  }

  cons_t *r = new cons_t();
  r->type = VECTOR;
  r->vector = v;
  return r;
}

cons_t* bytevector(size_t size, const uint8_t* fill)
{
  bytevector_t *v;

  if ( size )
    if ( fill ) v = new bytevector_t(size, *fill);
    else        v = new bytevector_t(size);
  else          v = new bytevector_t();

  cons_t *r = new cons_t();
  r->type = BYTEVECTOR;
  r->bytevector = v;
  return r;
}

cons_t* bytevector(const std::vector<uint8_t>& p)
{
  cons_t *r = new cons_t();
  r->type = BYTEVECTOR;
  r->bytevector = new bytevector_t(p);
  return r;
}

cons_t* bytevector(cons_t* p)
{
  uint8_t init = 0;
  cons_t* v = bytevector(length(p), &init);
  std::vector<uint8_t>& vec = v->bytevector->bytevector;

  for ( int i=0; !nullp(p); p = cdr(p), ++i ) {
    assert_type(INTEGER, car(p));

    int n = car(p)->integer;
    if ( n<0 || n>255 )
      raise(runtime_exception(
        "Bytevector elements must be integers in the range [0..255]"));

    vec[i] = static_cast<uint8_t>(n);
  }

  return v;
}

cons_t* car(const cons_t* p)
{
  return ( p != NULL && p->type == PAIR ) ? p->car : NULL;
}

cons_t* cdr(const cons_t* p)
{
  return ( p != NULL && p->type == PAIR ) ? p->cdr : NULL;
}

cons_t* caar(const cons_t* p)
{
  return car(car(p));
}

cons_t* caaar(const cons_t* p)
{
  return car(caar(p));
}

cons_t* cadr(const cons_t* p)
{
  return car(cdr(p));
}

cons_t* caadr(const cons_t* p)
{
  return car(cadr(p));
}

cons_t* caddr(const cons_t* p)
{
  return car(cddr(p));
}

cons_t* cddr(const cons_t* p)
{
  return cdr(cdr(p));
}

cons_t* cdddr(const cons_t* p)
{
  return cdr(cdr(cdr(p)));
}

cons_t* cadddr(const cons_t* p)
{
  return car(cdr(cdr(cdr(p))));
}

cons_t* caddddr(const cons_t* p)
{
  return car(cdr(cdr(cdr(cdr(p)))));
}

cons_t* cdaddr(const cons_t* p)
{
  return cdr(car(cdr(cdr(p))));
}

cons_t* caaddr(const cons_t* p)
{
  return car(car(cdr(cdr(p))));
}

cons_t* cdar(const cons_t* p)
{
  return cdr(car(p));
}

cons_t* cadar(const cons_t* p)
{
  return car(cdar(p));
}

cons_t* caddar(const cons_t* p)
{
  return car(cdr(cdar(p)));
}

cons_t* cadaar(const cons_t* p)
{
  return car(cdr(car(car(p))));
}

type_t type_of(const cons_t* p)
{
  return p == NULL ? NIL : p->type;
}

bool symbolp(const cons_t* p)
{
  return type_of(p) == SYMBOL;
}

bool atomp(const cons_t* p)
{
  return !pairp(p); // Queinnec, p. 4
}

bool integerp(const cons_t* p)
{
  return type_of(p) == INTEGER;
}

bool rationalp(const cons_t* p)
{
  return type_of(p) == RATIONAL;
}

bool realp(const cons_t* p)
{
  return type_of(p) == REAL;
}

bool vectorp(const cons_t* p)
{
  return type_of(p) == VECTOR;
}

bool bytevectorp(const cons_t* p)
{
  return type_of(p) == BYTEVECTOR;
}

bool portp(const cons_t* p)
{
  return type_of(p) == PORT;
}

bool environmentp(const cons_t* p)
{
  return type_of(p) == ENVIRONMENT;
}

bool charp(const cons_t* p)
{
  return type_of(p) == CHAR;
}

bool booleanp(const cons_t* p)
{
  return type_of(p) == BOOLEAN;
}

bool numberp(const cons_t* p)
{
  return integerp(p) || realp(p) || rationalp(p);
}

bool stringp(const cons_t* p)
{
  return type_of(p) == STRING;
}

bool nullp(const cons_t* p)
{
  return type_of(p) == NIL
    || (type_of(p)==PAIR && type_of(car(p))==NIL && type_of(cdr(p))==NIL);
}

bool pairp(const cons_t* p)
{
  // (1) A pair is a list, (2) except for the empty list '()
  return type_of(p) == PAIR &&         // (1)
    !(nullp(car(p)) && nullp(cdr(p))); // (2)
}

bool listp(const cons_t* p)
{
  /*
   * A _proper_ list is a pair whose cdr is a list.
   *
   * Note that this implementation is slow, because
   * it traverses the entire list.  Unfortunately,
   * I think this is needed. (TODO: Prove me wrong).
   *
   * Also note that we don't check for cycles here!
   */
  return type_of(p) == PAIR &&
      (nullp(cdr(p)) || listp(p->cdr));
}

bool properlistp(const cons_t* p)
{
  return type_of(p) == PAIR
    && !circularp(p) && listp(p);
}

bool closurep(const cons_t* p)
{
  return type_of(p) == CLOSURE;
}

bool syntaxp(const cons_t* p)
{
  return type_of(p) == SYNTAX;
}

bool equalp(const cons_t* l, const cons_t* r)
{
  return type_of(l) != type_of(r) ? false :
          print(l) == print(r); // <- SLOW, but sure
}

bool eqp(const cons_t* l, const cons_t* r)
{
  return eqvp(l, r);
}

/*
 * See R7RS chapters 6 and 6.3.3.
 *
 * For pairs, vectors, bytevectors, records or
 * strings, eqv? denotes they share the same
 * locations in the store (section 3.4).
 *
 */
bool eqvp(const cons_t* l, const cons_t* r)
{
  if ( type_of(l) != type_of(r) )
    return false;

  switch ( type_of(l) ) {
  case NIL:           return true;
  case BOOLEAN:       return l->boolean == r->boolean;
  case SYMBOL:        return l->symbol == r->symbol
                             || *(l->symbol) == *(r->symbol);
                      // TODO: Above, do we need *(l->symbol) check?
  case INTEGER:       return l->integer == r->integer;
                      // TODO: Should we also check exactness?
                         //    && l->exact == r->exact;

  case REAL:          // Check both exact/both inexact
                      return l->real == r->real;
  case CHAR:          return l->character == r->character;
  case PAIR:          return nullp(l) && nullp(r)? true : l == r;
  case VECTOR:        return l == r;
  case BYTEVECTOR:    return l == r;
  case RATIONAL:      return l == r;

                      // fast pointer comparison first
  case STRING:        return (l->string == r->string ||
                          !strcmp(l->string, r->string));
                      // TODO: I thought eqv? only compared pointers?
  case SYNTAX:        return l == r;
  case CLOSURE:       // double-check with section 6.1 and 4.1.4 (TODO)
                      return l->closure == r->closure;
  case CONTINUATION:  return l->continuation == r->continuation;
  case PORT:          return l->port == r->port ||
                             *l->port == *r->port;
  case ENVIRONMENT:   return l->environment == r->environment;
  case POINTER:       return l->pointer == r->pointer ||
                             *l->pointer == *r->pointer;
  }

  return false;
}

cons_t* append(cons_t *h, cons_t *t)
{
  return nullp(h)? t : cons(car(h), append(cdr(h), t));
}

cons_t* splice(cons_t *r, cons_t* p)
{
  cons_t *e = r;

  // skip `e´ to end of `p´
  while ( !nullp(cdr(e)) )
    e = cdr(e);

  while ( !nullp(p) ) {
    if ( nullp(car(e)) ) {
      e->car = !pairp(p)? p : car(p);
      e->cdr = cons(NULL);
    } else
      e->cdr = !pairp(p)? p : cons(car(p), cons(NULL));

    e = cdr(e);
    p = cdr(p);
  }

  return r;
}

cons_t* splice_into(cons_t *src, cons_t *dst)
{
  while ( !nullp(src) ) {
    dst->car = pairp(src)? car(src) : src;
    dst = dst->cdr = cons(NULL);
    src = cdr(src);
  }

  return dst;
}


cons_t* closure(lambda_t f, environment_t* e, bool syntactic)
{
  closure_t *c = new closure_t();
  c->function = f;
  c->environment = e;
  c->syntactic = syntactic;

  cons_t *p = new cons_t();
  p->type = CLOSURE;
  p->closure = c;

  return p;
}

size_t length(const cons_t *p)
{
  size_t n = 0;

  while ( !nullp(p) ) {
    p = cdr(p);
    ++n;
  }

  return n;
}

bool not_p(const cons_t* p)
{
  // all other types + values are considered true in scheme
  return booleanp(car(p)) && car(p)->boolean == false;
}

bool and_p(const cons_t* p)
{
  // implement in terms of not_p
  return !not_p(p) && !not_p(cdr(p));
}

bool or_p(const cons_t* p)
{
  // implement in terms of not_p
  return !not_p(p) || !not_p(cdr(p));
}

bool xor_p(const cons_t* p)
{
  return !not_p(p) ^ !not_p(cdr(p));
}

real_t number_to_real(const cons_t* p)
{
  assert_number(p);

  switch ( type_of(p) ) {
  default:
    raise(runtime_exception("Unsupported number->double conversion: " + sprint(p)));
  case INTEGER: return static_cast<real_t>(p->integer);
  case REAL:    return static_cast<real_t>(p->real);
  case RATIONAL: {
    real_t n = p->rational.numerator;
    real_t d = p->rational.denominator;
    return n / d;
  }}
}

bool iswhole(real_t  n)
{
  if ( !isfinite(n) || isnan(n) || !isnormal(n) )
    return false;

  // Return true if `n` has no decimals, i.e. is "x.0" for a value of x
  // NOTE: Can possible do `(int)n == n` as well, but better to use floor.
  return (floor(n) == n) && !(n <= INT_MIN || n >= INT_MAX);
}

int gcd(int a, int b)
{
  a = a<0? -a : a;
  b = b<0? -b : b;

  if ( a == 0 )
    return b;

  while ( b ) {
    if ( a > b )
      a -= b;
    else
      b -= a;
  }

  return a;
}

int lcm(int a, int b)
{
  return a*b / gcd(a, b);
}

cons_t* nil_coalesce(cons_t* p)
{
  // Always return list; empty list if null
  return !nullp(p)? p : list(NULL);
}

const std::string& symbol_name(const cons_t* p)
{
  static const std::string empty("");
  return !symbolp(p)? empty : *p->symbol;
}

bool emptylistp(const cons_t* p)
{
  return type_of(p) == PAIR &&
    p->car == NULL && p->cdr == NULL;
}

bool boolean_false(cons_t* p)
{
  return booleanp(p) && p->boolean == false;
}

environment_t* null_environment(int version)
{
  if ( version != 7 )
    raise(runtime_exception(format("Unsupported null environment version: %d", version)));

  environment_t *r = new environment_t();

  /*
   * Used to import(r, exports_import); here,
   * but that's handled by eval() now.
   */

  return r;
}

/*
 * Returns an empty environment with only import
 * defined.
 */
environment_t* null_import_environment()
{
  environment_t *r = new environment_t();
  import(r, exports_import);
  return r;
}

int decimals_in(real_t n)
{
  int r = 0;

  while ( (n - static_cast<int>(n)) != 0.0 ) {
    ++r;
    n *= 10.0;
  }

  return r;
}
