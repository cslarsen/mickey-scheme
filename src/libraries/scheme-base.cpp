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

#include <cmath>
#include "mickey.h"

extern "C" {

cons_t* proc_abs(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));

  if ( decimalp(car(p)) ) {
    decimal_t n = car(p)->decimal;
    return decimal(n<0.0? -n : n);
  }

  int n = car(p)->integer;
  return integer(n<0? -n : n);
}

cons_t* proc_current_output_port(cons_t *p, environment_t*)
{
  assert_length(p, 0);
  return port(&global_opts.current_output_device);
}

cons_t* proc_current_input_port(cons_t *p, environment_t*)
{
  assert_length(p, 0);
  return port(&global_opts.current_input_device);
}

cons_t* proc_current_error_port(cons_t *p, environment_t*)
{
  assert_length(p, 0);
  return port(&global_opts.current_error_device);
}

cons_t* proc_write(cons_t *p, environment_t*)
{
  // TODO: Check that current output device is a file pointer
  for ( ; !nullp(p); p = cdr(p) )
    fprintf(global_opts.current_output_device.file(),
      "%s", sprint(car(p)).c_str());

  return nil();
}

cons_t* proc_strcat(cons_t *p, environment_t*)
{
  std::string s;

  for ( ; !nullp(p); p = cdr(p) )
    s += print(car(p)).c_str();

  return string(s.c_str());
}

cons_t* proc_make_string(cons_t *p, environment_t*)
{
  assert_length(p, 1, 2);
  assert_type(INTEGER, car(p));

  size_t len = car(p)->integer;
  char   ch  = length(p)==1? ' ' : cadr(p)->character;

  return string(std::string(len, ch).c_str());
}

cons_t* proc_addf(cons_t *p, environment_t*)
{
  decimal_t sum = 0.0;

  for ( ; !nullp(p); p = cdr(p) ) {
    cons_t *i = listp(p)? car(p) : p;

    if ( integerp(i) )
      sum += static_cast<decimal_t>(i->integer);
    else if ( decimalp(i) )
      sum += i->decimal;
    else
      raise(runtime_exception("Cannot add decimal with " + to_s(type_of(i)) + ": " + sprint(i)));
  }

  return decimal(sum);
}

cons_t* proc_add(cons_t *p, environment_t* env)
{
  /*
   * Integers have an IDENTITY, so we can do this,
   * but a more correct approach would be to take
   * the value of the FIRST number we find and
   * return that.
   */
  int sum = 0;

  for ( ; !nullp(p); p = cdr(p) ) {
    cons_t *i = listp(p)? car(p) : p;

    if ( integerp(i) )
      sum += i->integer;
    else if ( decimalp(i) )
      // automatically convert; perform rest of computation in floats
      return proc_addf(cons(decimal(sum), p), env);
    else
      raise(runtime_exception("Cannot add integer with " + to_s(type_of(i)) + ": " + sprint(i)));
  }

  return integer(sum);
}

cons_t* proc_sub(cons_t *p, environment_t*)
{
  if ( length(p) == 0 )
    raise(runtime_exception("No arguments to -"));

  decimal_t d = number_to_float(car(p));

  // (- x) => -x, instead of +x
  if ( nullp(cdr(p)) )
    d = -d;

  while ( !nullp(p = cdr(p)) )
    d -= number_to_float(car(p));

  return iswhole(d) ? integer((int)d) : decimal(d);
}

cons_t* proc_divf(cons_t *p, environment_t*)
{
  assert_length(p, 2);

  cons_t *a = car(p);
  cons_t *b = cadr(p);

  decimal_t x = (type_of(a) == DECIMAL)? a->decimal : a->integer;
  decimal_t y = (type_of(b) == DECIMAL)? b->decimal : b->integer;

  // Automatically convert back to int if possible
  decimal_t q = x / y;
  return iswhole(q)? integer(static_cast<int>(q)) : decimal(q);
}

cons_t* proc_div(cons_t *p, environment_t *e)
{
  assert_length(p, 2);

  cons_t *a = car(p);
  cons_t *b = cadr(p);

  assert_number(a);
  assert_number(b);

  if ( integerp(a) && integerp(b) ) {
    if ( b->integer == 0 )
      raise(runtime_exception("Division by zero"));
    return integer(a->integer / b->integer);
  } else
    return proc_divf(p, e);
}

cons_t* proc_mulf(cons_t *p, environment_t*)
{
  decimal_t product = 1.0;

  for ( ; !nullp(p); p = cdr(p) ) {
    cons_t *i = listp(p)? car(p) : p;

    if ( integerp(i) )
      product *= static_cast<decimal_t>(i->integer);
    else if ( decimalp(i) )
      // automatically convert; perform rest of computation in floats
      product *= i->decimal;
    else
      raise(runtime_exception("Cannot multiply integer with " + to_s(type_of(i)) + ": " + sprint(i)));
  }

  return decimal(product);
}

cons_t* proc_mul(cons_t *p, environment_t *env)
{
  int product = 1;

  for ( ; !nullp(p); p = cdr(p) ) {
    cons_t *i = listp(p)? car(p) : p;

    if ( integerp(i) )
      product *= i->integer;
    else if ( decimalp(i) )
      // automatically convert; perform rest of computation in floats
      return proc_mulf(cons(decimal(product), p), env);
    else
      raise(runtime_exception("Cannot multiply integer with " + to_s(type_of(i)) + ": " + sprint(i)));
  }

  return integer(product);
}

cons_t* proc_to_string(cons_t* p, environment_t*)
{
  std::string s;

  for ( ; !nullp(p); p = cdr(p)) {
    if ( listp(car(p)) )
      s += print(car(p));
    else
      s += to_s(car(p));
  }

  return string(s.c_str());
}

cons_t* proc_list(cons_t* p, environment_t*)
{
  return nil_coalesce(p);
}

cons_t* proc_define(cons_t *p, environment_t *env)
{
  // (define <name> <body>)
  assert_type(SYMBOL, car(p));
  cons_t *name = car(p);
  cons_t *body = cadr(p);

  if ( name->symbol->name().empty() )
    raise(runtime_exception("Cannot define with empty variable name")); // TODO: Even possible?

  env->define(name->symbol->name(), body);
  return nil();
}

cons_t* proc_define_syntax(cons_t *p, environment_t *env)
{
  // (define <name> <body>)
  assert_type(SYMBOL, car(p));
  cons_t *name = car(p);
  cons_t *syntax = cadr(p);

  if ( name->symbol->name().empty() )
    raise(runtime_exception("Cannot define-syntax with empty name"));

  env->define(name->symbol->name(), syntax);
  return nil();
}

cons_t* proc_map(cons_t *p, environment_t*)
{
  /*
   * (map <procedure> <list1> <list2> ... <listN>)
   *
   * For each element in each list, do this:
   *
   * (<procedure>
   *    <elem1 from list1>
   *    <elem1 from list2> ... <elem1 from listN>)
   */

  assert_type(CLOSURE, car(p));

  // rest of args must be lists
  for ( cons_t* l = cdr(p); !nullp(l); l = cdr(l) )
    assert_type(PAIR, car(l));

  // Pointer-copy lists, since we'll mutate them
  cons_t *lists = list();
  for ( cons_t *l = cdr(p); !nullp(l); l = cdr(l) )
    lists = append(lists, list(car(l)));

  // perform <proc> on head of each list
  cons_t *result = list();
  cons_t *proc = car(p);

  while ( !nullp(lists) ) {
    cons_t *args = list();
    for ( cons_t *l = lists; !nullp(l); l = cdr(l) ) {
      args = append(args, cons(caar(l)));

      // terminate when shortest list is done
      if ( nullp(car(l)) || nullp(caar(l)) )
        return result;

      // make head point to next element
      l->car = cdar(l);
    }

    // eval (<proc> <head of list1> <head of list 2> ...)
    result = append(result, cons(
               apply(proc->closure->function, args, proc->closure->environment)));
  }

  return result;
}

cons_t* proc_cons(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  return cons(car(p), cadr(p));
}

cons_t* proc_car(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PAIR, car(p));
  return car(car(p));
}

cons_t* proc_cdr(cons_t* p, environment_t*)
{
  /*
   * NOTE:  We have a special (and potentially UGLY) case
   *        of doing "(cdr (list 1))" which should give "()",
   *        so we explicitly check for it here, although we
   *        probably SHOULD NOT (TODO).
   */
  assert_length(p, 1);
  assert_type(PAIR, car(p));
  cons_t *r = cdr(car(p));
  return nil_coalesce(r);
}

cons_t* proc_append(cons_t* p, environment_t*)
{
  cons_t *r = append(car(p), cadr(p));

  while ( !nullp(caddr(p)) ) {
    r = append(r, caddr(p));
    p = cddr(p);
  }
  
  return r;
}

cons_t* proc_symbolp(cons_t* p, environment_t*)
{
  return boolean(symbolp(car(p)));
}

cons_t* proc_integerp(cons_t* p, environment_t*)
{
  /*
   * Note that decimals like 3.0 should be considered
   * integers.
   */
  if ( decimalp(car(p)) ) {
    decimal_t n = car(p)->decimal;
    return boolean((decimal_t)((int)n) == n);
  }

  return boolean(integerp(car(p)));
}

cons_t* proc_realp(cons_t* p, environment_t*)
{
  /*
   * All integers can also be considered reals.
   */
  return boolean(
    decimalp(car(p)) ||
    integerp(car(p)));
}

cons_t* proc_nullp(cons_t* p, environment_t*)
{
  return boolean(nullp(car(p)));
}

cons_t* proc_zerop(cons_t* p, environment_t*)
{
  if ( type_of(car(p)) == INTEGER )
    return boolean(car(p)->integer == 0);

  if ( type_of(car(p)) == DECIMAL )
    return boolean(car(p)->decimal == 0.0);

  return boolean(false);
}

cons_t* proc_pairp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(pairp(car(p)));
}

cons_t* proc_close_port(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PORT, car(p));
  car(p)->port->close();
  return nil();
}

cons_t* proc_portp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(portp(car(p)));
}

cons_t* proc_port_openp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PORT, car(p));
  return boolean(car(p)->port->isopen());
}

cons_t* proc_input_portp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PORT, car(p));
  return boolean(car(p)->port->isreadable());
}

cons_t* proc_output_portp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PORT, car(p));
  return boolean(car(p)->port->iswritable());
}

cons_t* proc_textual_portp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PORT, car(p));
  return boolean(car(p)->port->istextual());
}

cons_t* proc_binary_portp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PORT, car(p));
  return boolean(car(p)->port->isbinary());
}

cons_t* proc_listp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(properlistp(car(p)));
}

cons_t* proc_numberp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(numberp(car(p)));
}

cons_t* proc_stringp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(stringp(car(p)));
}

cons_t* proc_procedurep(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(closurep(car(p)));
}

cons_t* proc_vectorp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(vectorp(car(p)));
}

cons_t* proc_bytevectorp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(bytevectorp(car(p)));
}

cons_t* proc_vector(cons_t* p, environment_t*)
{
  return vector(p);
}

cons_t* proc_make_vector(cons_t* p, environment_t*)
{
  assert_length(p, 1, 2);
  assert_type(INTEGER, car(p));

  size_t k = car(p)->integer;
  cons_t *fill = length(p)>1? cadr(p) : NULL;

  return !fill? vector(p, k) : vector(p, k, fill);
}

cons_t* proc_make_bytevector(cons_t* p, environment_t*)
{
  assert_length(p, 1, 2);
  assert_type(INTEGER, car(p));

  size_t k = car(p)->integer;
  cons_t *fill = length(p)>1? cadr(p) : NULL;

  if ( !fill ) {
    return bytevector(k);
  } else {
    uint8_t u8fill = 0;
    switch ( type_of(fill) ) {
    case BOOLEAN: u8fill = fill->boolean; break;
    case CHAR:    u8fill = fill->character; break;
    case INTEGER: {
      if ( fill->integer < 0 || fill->integer > 255 )
        raise(runtime_exception(
          "make-vector only accepts unsigned 8-bit bytes"));
      u8fill = static_cast<uint8_t>(fill->integer);
      break;
    }

    default:
      raise(runtime_exception(
        "make-vector only accepts unsigned 8-bit bytes"));
      break;
    }

    return bytevector(k, &u8fill);
  }
}

cons_t* proc_vector_length(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(VECTOR, car(p));
  return integer(static_cast<int>(car(p)->vector->vector.size()));
}

cons_t* proc_vector_ref(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(VECTOR, car(p));
  assert_type(INTEGER, cadr(p));

  vector_t *v = car(p)->vector;
  int ref = cadr(p)->integer;

  if ( ref<0 || static_cast<size_t>(ref) >= v->vector.size() )
    raise(runtime_exception("vector-ref index out of range: " + to_s(ref)));

  return v->vector[ref];
}

cons_t* proc_vector_set(cons_t* p, environment_t*)
{
  assert_length(p, 3);
  assert_type(VECTOR, car(p));
  assert_type(INTEGER, cadr(p));

  vector_t *v = car(p)->vector;
  int ref = cadr(p)->integer;
  cons_t *set = caddr(p);

  if ( ref<0 || static_cast<size_t>(ref) >= v->vector.size() )
    raise(runtime_exception("Vector index out of range: " + to_s(ref)));

  v->vector[ref] = set;
  return nil();
}

cons_t* proc_vector_to_list(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(VECTOR, car(p));

  vector_t *v = car(p)->vector;
  const std::vector<cons_t*>& vec = v->vector;
  cons_t *ret = list();
  cons_t *r = ret;

  for ( std::vector<cons_t*>::const_iterator i = vec.begin();
        i != vec.end(); ++i )
  {
    r = r->cdr = cons(*i);
  }

  return cdr(ret);
}

cons_t* proc_vector_to_string(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(VECTOR, car(p));

  const std::vector<cons_t*>& vec = car(p)->vector->vector;
  std::string s(vec.size(), '\0');

  size_t n=0;
  for ( std::vector<cons_t*>::const_iterator i = vec.begin();
        i != vec.end(); ++i )
  {
    if ( type_of(*i) != CHAR )
      raise(runtime_exception(
        "vector->string requires character elements only"));

    s[n++] = (*i)->character;
  }

  return string(s.c_str());
}

cons_t* proc_list_to_vector(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PAIR, car(p));

  cons_t *r = new cons_t();
  r->type = VECTOR;
  r->vector = new vector_t(length(car(p)));

  size_t n=0;

  for ( p = car(p); !nullp(p); p = cdr(p) )
    r->vector->vector[n++] = car(p);

  return r;
}

cons_t* proc_vector_copy(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(VECTOR, car(p));

  cons_t *r = new cons_t();
  r->type = VECTOR;
  r->vector = new vector_t(*car(p)->vector);
  return r;
}

cons_t* proc_bytevector_copy(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(BYTEVECTOR, car(p));

  cons_t *r = new cons_t();
  r->type = BYTEVECTOR;
  r->bytevector = new bytevector_t(*car(p)->bytevector);
  return r;
}

cons_t* proc_bytevector_u8_ref(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(BYTEVECTOR, car(p));
  assert_type(INTEGER, cadr(p));

  bytevector_t *v = car(p)->bytevector;
  int k = cadr(p)->integer;

  if ( k<0 || static_cast<size_t>(k) >= v->bytevector.size() )
    raise(runtime_exception("bytevector-u8-ref out of range"));

  return integer(v->bytevector[k]);
}

cons_t* proc_bytevector_length(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(BYTEVECTOR, car(p));
  return integer(static_cast<int>(car(p)->bytevector->bytevector.size()));
}

cons_t* proc_bytevector_copy_partial(cons_t* p, environment_t*)
{
  assert_length(p, 3);
  assert_type(BYTEVECTOR, car(p));
  assert_type(INTEGER, cadr(p));
  assert_type(INTEGER, caddr(p));

  int start = cadr(p)->integer;
  int end = caddr(p)->integer;

  bytevector_t *v = car(p)->bytevector;

  if ( start<0 || static_cast<size_t>(start)>=v->bytevector.size() )
    raise(runtime_exception("bytevector-copy-partial `start´ out of range"));

  if ( end<=start || static_cast<size_t>(end)>v->bytevector.size() )
    raise(runtime_exception("bytevector-copy-partial `end´ out of range"));

  return bytevector(
    std::vector<uint8_t>(
      v->bytevector.begin()+start,
      v->bytevector.begin()+end));
}

cons_t* proc_bytevector_copy_partial_bang(cons_t* p, environment_t*)
{
  assert_length(p, 5);
  assert_type(BYTEVECTOR, car(p));
  assert_type(INTEGER, cadr(p));
  assert_type(INTEGER, caddr(p));
  assert_type(BYTEVECTOR, cadddr(p));
  assert_type(INTEGER, caddddr(p));

  int start = cadr(p)->integer;
  int end   = caddr(p)->integer;
  int at    = caddddr(p)->integer;

  bytevector_t *from = car(p)->bytevector;
  bytevector_t *to = cadddr(p)->bytevector;

  if ( start<0 || static_cast<size_t>(start) >= from->bytevector.size() )
    raise(runtime_exception("bytevector-copy-partial `start´ out of range"));

  if ( end<=start || static_cast<size_t>(end) > from->bytevector.size() )
    raise(runtime_exception("bytevector-copy-partial `end´ out of range"));

  if ( !(static_cast<int>((to->bytevector.size()) - at) >= (end - start)) )
    raise(runtime_exception(
      "bytevector-copy-partial! invalid start-end-at combo"));

  std::copy(
    from->bytevector.begin()+start,
    from->bytevector.begin()+end,
    to->bytevector.begin()+at);

  return nil();
}

cons_t* proc_bytevector_u8_set_bang(cons_t* p, environment_t*)
{
  assert_length(p, 3);
  assert_type(BYTEVECTOR, car(p));
  assert_type(INTEGER, cadr(p));
  assert_type(INTEGER, caddr(p));

  bytevector_t *v = car(p)->bytevector;
  int k = cadr(p)->integer;
  int val = caddr(p)->integer;

  if ( k<0 || static_cast<size_t>(k) >= v->bytevector.size() )
    raise(runtime_exception("bytevector-u8-set! index out of range"));

  if ( val<0 || val>255 )
    raise(runtime_exception("bytevector-u8-set! byte value out of range"));

  v->bytevector[k] = static_cast<uint8_t>(val);
  return nil();
}

cons_t* proc_bytevector_copy_bang(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(BYTEVECTOR, car(p));
  assert_type(BYTEVECTOR, cadr(p));

  bytevector_t *from = car(p)->bytevector;
  bytevector_t *to   = cadr(p)->bytevector;

  if ( to->bytevector.size() < from->bytevector.size() )
    raise(runtime_exception(
      "bytevector-copy! destination bytevector shorter than source"));

  if ( to->bytevector.size() > from->bytevector.size() )
    to->bytevector.resize(from->bytevector.size());

  to->bytevector = from->bytevector;
  return nil();
}

cons_t* proc_vector_fill(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(VECTOR, car(p));

  cons_t *fill = cadr(p);
  vector_t *v = car(p)->vector;

  for ( std::vector<cons_t*>::iterator i = v->vector.begin();
        i != v->vector.end();
        ++i )
  {
    (*i) = fill;
  }

  return nil();
}

cons_t* proc_string_to_vector(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(STRING, car(p));

  cons_t *r = new cons_t();
  r->type = VECTOR;
  r->vector = new vector_t(strlen(car(p)->string));

  size_t n=0;
  const char* s = car(p)->string;
  while ( *s )
    r->vector->vector[n++] = character(*s++);

  return r;
}

cons_t* proc_charp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(charp(car(p)));
}

cons_t* proc_booleanp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(booleanp(car(p)));
}

cons_t* proc_length(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PAIR, car(p));
  assert_noncyclic(car(p));
  return integer(static_cast<int>(length(car(p))));
}

cons_t* proc_eqp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  return boolean(eqp(car(p), cadr(p)));
}

cons_t* proc_eqvp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  return boolean(eqvp(car(p), cadr(p)));
}

cons_t* proc_equalp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  return boolean(equalp(car(p), cadr(p)));
}

cons_t* proc_eqintp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_number(car(p));
  assert_number(cadr(p));

  cons_t *l = car(p),
         *r = cadr(p);

  return (decimalp(l) || decimalp(r)) ?
    boolean(number_to_float(l) == number_to_float(r)) :
    boolean(l->integer == r->integer);
}

cons_t* proc_not(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(not_p(p));
}

/*
 * If any expression evaluates to #f,
 * stop and return #f.  If not, return
 * value of last expression.
 */
cons_t* proc_and(cons_t* p, environment_t*)
{
  /*
   * (and) should return #t.
   */
  cons_t* last = boolean(true);

  for(;;) {
    if ( nullp(p) )
      return last;

    last = car(p);

    if ( not_p(p) )
      return boolean(false);

    p = cdr(p);
  }

  return last;
}

/*
 * Evaluate from left to right, and return
 * first expression that returns true, disregarding
 * the rest.
 */
cons_t* proc_or(cons_t* p, environment_t*)
{
  /*
   * (or) should return #t.
   */
  for(;;) {
    if ( nullp(p) )
      break;

    // true? then stop and return it
    if ( !not_p(p) )
      return car(p);

    p = cdr(p);
  }

  return boolean(false);
}

cons_t* proc_xor(cons_t* p, environment_t*)
{
  return boolean(xor_p(p));
}

cons_t* proc_less(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_number(car(p));
  assert_number(cadr(p));

  decimal_t x = (type_of(car(p)) == INTEGER)? car(p)->integer : car(p)->decimal;
  decimal_t y = (type_of(cadr(p)) == INTEGER)? cadr(p)->integer : cadr(p)->decimal;

  return boolean(x < y);
}

cons_t* proc_greater(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_number(car(p));
  assert_number(cadr(p));

  decimal_t x = (type_of(car(p)) == INTEGER)? car(p)->integer : car(p)->decimal;
  decimal_t y = (type_of(cadr(p)) == INTEGER)? cadr(p)->integer : cadr(p)->decimal;

  return boolean(x > y);
}

cons_t* proc_reverse(cons_t* p, environment_t*)
{
  cons_t *r = NULL;

  for ( p = car(p); !nullp(p); p = cdr(p) )
    r = cons(car(p), r);

  return r;
}

cons_t* proc_let(cons_t* p, environment_t* e)
{
  /*
   * Transform to lambdas:
   *
   * (let <name>
   *      ((name-1 value-1)
   *       (name-2 value-2)
   *       (name-n value-n))
   *       <body>)
   *
   * to
   *
   * ((lambda (name-1 name-2 name-n)
   *    <body>) value-1 value-2 value-n)
   *
   * If an OPTIONAL <name> is given, we have a "named let"
   * which will produce the output:
   *
   * (letrec
   *   ((<name> (lambda
   *              (name-1 name-2 name-n)
   *              <body>)))
   *   (<name> value-1 value-2 value-n))
   *
   */

  cons_t *name = NULL;

  // named let?
  if ( symbolp(car(p)) ) {
    name = car(p);
       p = cdr(p);
  }

  cons_t *body = cdr(p),
        *names = list(NULL),
       *values = list(NULL);

  for ( cons_t *n = car(p); !nullp(n); n = cdr(n) ) {
     names = append(names, list(caar(n)));
    values = append(values, list(car(cdar(n))));
  }

  /*
   * Normal let:
   *
   * Build lambda expression and return it, eval will eval it :)
   * (or we could call make_closure here):
   *
   * ((lambda (<names>) <body>) <values>)
   *
   */
  if ( !name )
    return cons(cons(symbol("lambda", e),
           cons(names, cons(proc_begin(body, e)))), values);

  /*
   * Transform named let to letrec.
   */
  return cons(symbol("letrec", e),            // (letrec
           cons(cons(cons(name,               //   ((<name>
             cons(cons(symbol("lambda", e),   //      (lambda
               cons(names,                    //        (name-1 .. name-n)
               cons(proc_begin(body, e))))))),//        (begin <body>))))
           cons(cons(name, values))));        //   (<name> value-1 .. value-n))
}

cons_t* proc_letstar(cons_t* p, environment_t* e)
{
  /*
   * Transform to nested lambdas:
   *
   * (let* ((name-1 value-1)
   *        (name-2 value-2)
   *        (name-3 value-3))
   *        <body>)
   * to
   *
   * ((lambda (name-1)
   * ((lambda (name-2)
   * ((lambda (name-3)
   *   <body>) value-3))
   *           value-2))
   *           value-1)
   */

  cons_t  *body  = cdr(p),
          *names = list(NULL),
         *values = list(NULL);

  for ( cons_t *n = car(p); !nullp(n); n = cdr(n) ) {
     names = cons(caar(n), names);
    values = cons(cadar(n), values);
  }

  // Work our way outward by constructing lambdas
  cons_t *inner = proc_begin(body, e);

  while ( !nullp(names) && !nullp(values) ) {
    inner = cons(cons(symbol("lambda", e),
      cons(cons(car(names)), cons(inner))), cons(car(values)));

     names = cdr(names);
    values = cdr(values);
  }

  return inner;
}

cons_t* proc_letrec(cons_t* p, environment_t* e)
{
  /*
   * Transform to begin-define with dummy initial values
   * and using set! to update with correct ones:
   *
   * (letrec ((name-1 value-1)
   *          (name-2 value-2)
   *          (name-3 value-3))
   *         <body>)
   * to
   *
   * (let ((name-1 #f)
   *       (name-2 #f)
   *       (name-3 #f))
   *   (set! name-1 value1)
   *   (set! name-2 value2)
   *   (set! name-3 value3)
   *    <body>)
   */

  cons_t  *body  = cdr(p),
          *names = list(NULL),
         *values = list(NULL);

  for ( cons_t *n = car(p); !nullp(n); n = cdr(n) ) {
     names = cons(caar(n), names);
    values = cons(cadar(n), values);
  }

  // do not clutter other environments
  cons_t *r = NULL;

  // (define name-N #f)
  for ( cons_t *n=names; !nullp(n); n=cdr(n) )
    r = append(r, cons(cons(car(n),
                       cons(boolean(false)))));

  // wrap in: (let ((key value) ...))
  r = cons(symbol("let", e), list(r));

  // (set! name-N value-N)
  for ( cons_t *n=names,
               *v=values;
        !nullp(n) && !nullp(v);
        n=cdr(n), v=cdr(v) )
  {
    r = append(r, cons(cons(symbol("set!", e),
                              cons(car(n),
                              cons(car(v))))));
  }

  // add <body>
  r = append(r, body);

  return r;
}

/*
 * Utility function used internally in exported functions.
 */
static cons_t* let(cons_t *bindings, cons_t *body, environment_t *e)
{
  return cons(symbol("let", e), cons(bindings, cons(body)));
}

cons_t* proc_cond(cons_t* p, environment_t* e)
{
  /*
   * Transform:
   *
   * (cond ((case-1) action-1 ...)
   *       ((case-2) action-2 ...)
   *       ((case-n) action-n ...)
   *       (else <else action>))
   *
   * to
   *
   *  (let ((result <case-1>))
   *    (if result (begin action-1 ...)
   *      (let ((result <case-2>))
   *        (if result (begin <action-2> ...)
   *          (let ((result <case-n>))
   *            (if result (begin <action-n>))
   *              <else action>)))))
   *
   * Also, forms involving the literal `=>´ will be
   * transformed like the following:
   *
   *  ((<case> => <action>))
   *
   * is transformed to
   *
   * (begin (<action> result))
   *
   */

  cons_t *r = list(NULL);
  p = cdr(p);

  if ( nullp(p) )
    return r;

  cons_t   *test = caar(p),
         *action = car(cdar(p));

  if ( symbol_name(action) == "=>" )
    action = cons(cadr(cdar(p)), cons(symbol("result", e)));

  cons_t *otherwise = proc_cond(p, e);

  return (symbol_name(test) == "else")? action :
    let(list(list(symbol("result", e), test)), // (let ((result <test>))
      cons(symbol("if", e),                    //   (if result
        cons(symbol("result", e),              //     (begin <action>)
          cons(cons(symbol("begin", e),        //       <otherwise>))
            cons(action)),
              cons(otherwise)))), e);
}

cons_t* proc_number_to_string(cons_t* p, environment_t* e)
{
  assert_length(p, 1, 2);
  assert_number(car(p));

//  int radix = 10;
  if ( !nullp(cadr(p)) ) {
    assert_type(INTEGER, cadr(p));
    //radix = cadr(p)->integer;
  }

  // TODO: Implement use of radix
  return proc_to_string(cons(car(p)), e);
}

cons_t* proc_symbol_to_string(cons_t* p, environment_t* e)
{
  assert_length(p, 1);
  assert_type(SYMBOL, car(p));
  return proc_to_string(p, e);
}

cons_t* proc_boolean_to_string(cons_t* p, environment_t* e)
{
  assert_length(p, 1);
  assert_type(BOOLEAN, car(p));
  return proc_to_string(p, e);
}

/*
 * See comments in proc_set_cdr.
 */
cons_t* proc_set_car(cons_t* p, environment_t* e)
{
  cons_t *target = car(p);
  cons_t *source = cadr(p);

  if ( type_of(target) == SYMBOL )
    target = e->lookup_or_throw(target->symbol->name());

  if ( type_of(target) != PAIR )
    assert_type(PAIR, target); // raise error

  if ( type_of(source) == SYMBOL )
    source = e->lookup_or_throw(source->symbol->name());
  else // constant, or whatever
    source = eval(source, e);

  target->car = source;
  return nil();
}

/*
 * set-cdr! is intercepted by eval, so we can look up
 * addresses ourselves.
 *
 * We do it this way so we can create circular cells.
 */
cons_t* proc_set_cdr(cons_t* p, environment_t* e)
{
  cons_t *target = car(p);
  cons_t *source = cadr(p);

  if ( type_of(target) == SYMBOL )
    target = e->lookup_or_throw(target->symbol->name());

  if ( type_of(target) != PAIR )
    assert_type(PAIR, target); // raise error

  if ( type_of(source) == SYMBOL )
    source = e->lookup_or_throw(source->symbol->name());
  else // constant, or whatever
    source = eval(source, e);

  target->cdr = source;
  return nil();
}

cons_t* proc_file_existsp(cons_t* p, environment_t*)
{
  return boolean(file_exists(car(p)->string));
}

cons_t* proc_begin(cons_t* p, environment_t* e)
{
  return cons(symbol("begin", e), p);
}

cons_t* proc_gteq(cons_t* p, environment_t* e)
{
  return boolean(proc_eqintp(p, e)->boolean || proc_greater(p, e)->boolean);
}

cons_t* proc_lteq(cons_t* p, environment_t* e)
{
  return boolean(proc_eqintp(p, e)->boolean || proc_less(p, e)->boolean);
}

cons_t* proc_assq(cons_t* p, environment_t* e)
{
  assert_length(p, 2);

  cons_t *find = car(p),
        *alist = cadr(p);

  for ( p = alist; !nullp(p); p = cdr(p) )
    if ( proc_eqp(list(find, caar(p)), e)->boolean )
      return car(p);

  // Not found
  return boolean(false);
}

cons_t* proc_assv(cons_t* p, environment_t* e)
{
  assert_length(p, 2);

  cons_t *find = car(p),
        *alist = cadr(p);

  for ( p = alist; !nullp(p); p = cdr(p) )
    if ( proc_eqvp(list(find, caar(p)), e)->boolean )
      return car(p);

  // Not found
  return boolean(false);
}

cons_t* proc_assoc(cons_t* p, environment_t* e)
{
  assert_length(p, 2);

  cons_t *find = car(p),
        *alist = cadr(p);

  for ( p = alist; !nullp(p); p = cdr(p) )
    if ( proc_equalp(list(find, caar(p)), e)->boolean )
      return car(p);

  // Not found
  return boolean(false);
}

cons_t* proc_evenp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));
  return boolean(!(car(p)->integer & 1));
}

cons_t* proc_error(cons_t* p, environment_t*)
{
  /*
   * Note: This is an incomplete implementation of
   * (error).  TODO: Fix this.
   */
  assert_length_min(p, 1);
  assert_type(STRING, car(p));
  const char *message = car(p)->string;
  raise(runtime_exception(message));
  return nil();
}

cons_t* proc_oddp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));
  return boolean(car(p)->integer & 1);
}

cons_t* proc_negativep(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));
  return boolean(integerp(car(p)) ? car(p)->integer < 0 :
                                    car(p)->decimal < 0);
}

cons_t* proc_newline(cons_t* p, environment_t*)
{
  assert_length(p, 0, 1);

  port_t *po = &global_opts.current_output_device;

  if ( length(p) == 1 ) {
    assert_type(PORT, car(p));
    po = car(p)->port;
  }

  if ( !po->iswritable() )
    raise(runtime_exception("Port is not writable: " + to_s(po)));

  if ( po->fileport() ) {
    fprintf(po->file(), "\n");

    /*
     * Can just as well flush it when we have
     * a newline (even though it slows everything
     * down, it's kinda neat to flush at these
     * points.
     */
    fflush(po->file());
  } else
    raise(runtime_exception("String ports are not supported: " + to_s(po)));

  return nil();
}

cons_t* proc_positivep(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));
  return boolean(integerp(car(p)) ? car(p)->integer > 0 :
                                    car(p)->decimal > 0);
}

cons_t* proc_round(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));

  if ( integerp(car(p)) )
    return integer(car(p)->integer);
  else
    return decimal(roundf(car(p)->decimal));
}

cons_t* proc_truncate(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));

  if ( integerp(car(p)) )
    return integer(car(p)->integer);
  else
    return decimal(truncf(car(p)->decimal));
}

cons_t* proc_min(cons_t* p, environment_t*)
{
  assert_length_min(p, 1);
  cons_t *min = car(p);

  while ( !nullp(p) ) {
    assert_number(car(p));

    if ( number_to_float(car(p)) < number_to_float(min) )
      min = car(p);

    p = cdr(p);
  }

  return min;
}

cons_t* proc_max(cons_t* p, environment_t*)
{
  assert_length_min(p, 1);
  cons_t *max = car(p);

  while ( !nullp(p) ) {
    assert_number(car(p));

    if ( number_to_float(car(p)) > number_to_float(max) )
      max = car(p);

    p = cdr(p);
  }

  return max;
}

cons_t* proc_expt(cons_t* p, environment_t*)
{
  assert_length(p, 2);

  cons_t *base = car(p),
         *expn = cadr(p);

  assert_number(base);
  assert_number(expn);

  bool exact = integerp(base) && integerp(expn);

  if ( exact ) {
    int a = base->integer,
        n = expn->integer,
        r = a;

    // Per definition
    if ( n == 0 )
      return integer(1);

    if ( n < 0 )
      raise(runtime_exception("Negative exponents not implemented"));

    // This is a slow version
    // TODO: Implement O(log n) version
    while ( n-- > 1 )
      r *= a;

    return integer(r);
  }

  // Floating point exponentiation
  decimal_t a = number_to_float(base),
            n = number_to_float(expn),
            r = a;

  if ( n == 0.0 )
    return decimal(1.0);

  if ( n < 0.0 )
    raise(runtime_exception("Negative exponents not implemented"));

  while ( floor(n) > 1.0 ) {
    r *= a;
    n -= 1.0;
  }

  if ( n > 1.0 )
    raise(runtime_exception("Fractional exponents not supported"));

  // TODO: Compute r^n, where n is in [0..1)
  return decimal(r);
}

cons_t* proc_modulo(cons_t* p, environment_t*)
{
  assert_length(p, 2);

  cons_t *a = car(p),
         *b = cadr(p);

  assert_type(INTEGER, a);
  assert_type(INTEGER, b);

  if ( b->integer == 0 )
    raise(runtime_exception("Division by zero"));

  if ( b->integer < 0 )
    raise(runtime_exception("Negative modulus operations not implemented")); // TODO

  return integer(a->integer % b->integer);
}

cons_t* proc_char_to_integer(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(CHAR, car(p));
  return integer(static_cast<int>(car(p)->character));
}

cons_t* proc_char_ltep(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(CHAR, car(p));
  assert_type(CHAR, cadr(p));
  return boolean(car(p)->character <= cadr(p)->character);
}

cons_t* proc_char_ltp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(CHAR, car(p));
  assert_type(CHAR, cadr(p));
  return boolean(car(p)->character < cadr(p)->character);
}

cons_t* proc_char_eqp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(CHAR, car(p));
  assert_type(CHAR, cadr(p));
  return boolean(car(p)->character == cadr(p)->character);
}

cons_t* proc_char_gtp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(CHAR, car(p));
  assert_type(CHAR, cadr(p));
  return boolean(car(p)->character > cadr(p)->character);
}

cons_t* proc_char_gtep(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(CHAR, car(p));
  assert_type(CHAR, cadr(p));
  return boolean(car(p)->character >= cadr(p)->character);
}

cons_t* proc_integer_to_char(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));
  return character(static_cast<char>(car(p)->integer));
}

cons_t* proc_list_to_string(cons_t* p, environment_t*)
{
  assert_type(PAIR, car(p));
  p = car(p);

  // list of char -> string
  std::string s;

  while ( !nullp(p) ) {
    assert_type(CHAR, car(p));
    s += car(p)->character;
    p = cdr(p);
  }

  return string(s.c_str());
}

cons_t* proc_list_tail(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(PAIR, car(p));
  assert_type(INTEGER, cadr(p));

  size_t n = cadr(p)->integer;
  p = car(p);

  if ( n > length(p) )
    raise(runtime_exception("List is too short for your list-tail argument"));

  while ( n-- )
    p = cdr(p);

  return nil_coalesce(p);
}

cons_t* proc_list_ref(cons_t* p, environment_t* e)
{
  return car(proc_list_tail(p, e));
}

/*
 * Used by member, memq and memv
 */
static cons_t* proc_member_fptr(cons_t* p, environment_t*, bool (*compare)(const cons_t*, const cons_t*))
{
  assert_length(p, 2);
  assert_type(PAIR, cadr(p));

  cons_t *needle = car(p),
       *haystack = cadr(p);

  while ( !nullp(haystack) ) {
    if ( compare(needle, car(haystack)) )
      return haystack;

    haystack = cdr(haystack);
  }

  return boolean(false);
}

cons_t* proc_member(cons_t* p, environment_t* e)
{
  return proc_member_fptr(p, e, equalp);
}

cons_t* proc_memv(cons_t* p, environment_t* e)
{
  return proc_member_fptr(p, e, eqvp);
}

cons_t* proc_memq(cons_t* p, environment_t* e)
{
  return proc_member_fptr(p, e, eqp);
}

cons_t* proc_lcm(cons_t* p, environment_t* e)
{
  switch ( length(p) ) {
  case 0:
    return integer(1);

  case 1:
    assert_type(INTEGER, car(p));
    return integer(abs(car(p)->integer));

  case 2: {
    assert_type(INTEGER, cadr(p));

    int a = abs(car(p)->integer),
        b = abs(cadr(p)->integer);

    return integer(lcm(a, b));
  }

  default: {
    /*
     * We have at least 3 numbers; handle recursively, since
     * lcm(a, b, c) = lcm(lcm(a, b), c)
     */
    cons_t *r = car(p);
    p = cdr(p);

    while ( !nullp(p) ) {
      r = proc_lcm(list(r, car(p)), e);
      p = cdr(p);
    }

    return integer(r->integer);
  } }
}

cons_t* proc_string_to_list(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(STRING, car(p));

  cons_t *r = list(NULL);

  for ( const char* s = car(p)->string; *s; ++s )
    r = append(r, list(character(*s)));

  return r;
}

cons_t* proc_string_to_symbol(cons_t* p, environment_t* e)
{
  assert_length(p, 1);
  assert_type(STRING, car(p));
  return symbol(car(p)->string, e);
}

cons_t* proc_string_to_number(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(STRING, car(p));

  const char *s = car(p)->string;

  if ( isfloat(s) )
    return decimal(to_f(s));
  else if ( isinteger(s) )
    return integer(to_i(s));

  return boolean(false);
}

cons_t* proc_string_length(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(STRING, car(p));
  return integer(static_cast<int>(strlen(car(p)->string)));
}

cons_t* proc_string(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(CHAR, car(p));
  char s[2] = {car(p)->character, '\0'};
  return string(s);
}

cons_t* proc_substring(cons_t* p, environment_t*)
{
  assert_length(p, 3);

  cons_t *str = car(p),
       *start = cadr(p),
         *len = caddr(p);

  assert_type(STRING, str);
  assert_type(INTEGER, start);
  assert_type(INTEGER, len);

  return string(std::string(str->string).
      substr(start->integer,
             len->integer).c_str());
}

cons_t* proc_string_ltp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(STRING, car(p));
  assert_type(STRING, cadr(p));
  return boolean(strcmp(car(p)->string, cadr(p)->string) < 0);
}

cons_t* proc_string_ltep(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(STRING, car(p));
  assert_type(STRING, cadr(p));
  return boolean(strcmp(car(p)->string, cadr(p)->string) <= 0);
}

cons_t* proc_string_eqp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(STRING, car(p));
  assert_type(STRING, cadr(p));
  return boolean(strcmp(car(p)->string, cadr(p)->string) == 0);
}

cons_t* proc_string_gtep(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(STRING, car(p));
  assert_type(STRING, cadr(p));
  return boolean(strcmp(car(p)->string, cadr(p)->string) >= 0);
}

cons_t* proc_string_gtp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(STRING, car(p));
  assert_type(STRING, cadr(p));
  return boolean(strcmp(car(p)->string, cadr(p)->string) > 0);
}

cons_t* proc_string_ref(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(STRING, car(p));
  assert_type(INTEGER, cadr(p));

  if ( static_cast<size_t>(cadr(p)->integer) >= strlen(car(p)->string) )
    raise(runtime_exception("string-ref argument out of bounds"));

  return character(car(p)->string[ cadr(p)->integer]);
}

cons_t* proc_nanp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));

  if ( type_of(car(p)) == INTEGER )
    return boolean(false);

  return boolean(std::isnan(car(p)->decimal));
}

cons_t* proc_infinitep(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));

  if ( type_of(car(p)) == INTEGER )
    return boolean(false);

  return boolean(std::fpclassify(car(p)->decimal) == FP_INFINITE);
}

cons_t* proc_finitep(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));

  if ( type_of(car(p)) == INTEGER )
    return boolean(true);

  return boolean(std::isfinite(car(p)->decimal));
}

cons_t* proc_do(cons_t* p, environment_t* e)
{
  /*
   * Expand according to r7rs, pp. 56:
   *
   *  (letrec
   *    ((loop (lambda (<var1> <var2> <varN>) 
   *         (if <test>
   *             (begin
   *               (if #f #f) ; used to get unspecified value
   *               <expr>)
   *             (begin
   *               <command>
   *               (loop (<step1> <step2> <stepN>)))))))
   *    (loop <init1> <init2> <initN>))
   */

  cons_t *vars = cadr(p),
         *test = caaddr(p),
         *test_body = cons(symbol("begin",e), cdaddr(p)),
         *body = cons(symbol("begin",e), cdddr(p));

  // find variable names, initial values and stgeps
  cons_t *names = list(NULL),
         *init  = list(NULL),
         *step  = list(NULL);

  for ( cons_t *n = vars; !nullp(n); n = cdr(n) ) {
    names = append(names, cons(caar(n)));
    init  = append(init, cons(cadar(n)));
    step  = append(step, cons(caddar(n)));
  }

  // (loop <step1> <step2> <stepN>)
  cons_t* loop = cons(symbol("loop", e));
  for ( cons_t *n = step; !nullp(n); n = cdr(n) )
    loop = append(loop, cons(car(n)));

  // (begin <body> (loop ...))
  body = append(body, cons(loop));

  // (if <test> <test_body> (begin <body> (loop ...))
  cons_t *if_expr =
    cons(symbol("if", e),
      cons(test,
        cons(test_body,
          cons(body))));

  // (lambda (<var1> <var2> <varN>) <if_expr>)
  cons_t *lambda =
    cons(symbol("lambda", e),
      cons(names,
        cons(if_expr)));

  // (loop <lambda>)
  loop =
    cons(symbol("loop", e),
      cons(lambda));

  // (loop <init1> <init2> <initN>)
  cons_t *loop_init =
    cons(symbol("loop", e),
      init);

  // (letrec ((loop <loop_body)) (loop <init1> ...))
  cons_t *letrec =
    cons(symbol("letrec", e),
      cons(list(loop),
       cons(loop_init)));

  return letrec;
}

cons_t* proc_case(cons_t *p, environment_t* e)
{
  /*
   * Transforms
   *
   *  (case <key>
   *    ((<datum> ...) <expression> ...)
   *    ((<datum> ...) => <expression>)
   *    (else <expression> ...) ; OR
   *    (else => <expression>))
   *
   * to the code
   *
   *  (let
   *    ((value <key>))
   *    (cond
   *      ((memv value (quote (<datum> ...))) <expression> ...)
   *      ((memv value (quote (<datum> ...))) => <expression>)
   *      (else <expression>) ; OR
   *      (else => <expression>)))
   *
   */

  bool has_else = false;
  cons_t *key = cadr(p);
  cons_t *clauses = cons(NULL);

  for ( cons_t *c = cddr(p); !nullp(c); c = cdr(c) ) {
    cons_t *datum = caar(c);
    cons_t *exprs = cdar(c);

    if ( symbol_name(datum) == "else" ) {
      if ( symbol_name(cadar(c)) == "=>" ) {
        // produces: (else (<expression / procedure> <key>))
        clauses = append(clauses,
                      cons(cons(symbol("else", e),
                        cons(cons(cadr(cdar(c)),
                          cons(symbol("value", e)))))));
      } else
        clauses = append(clauses, c);

      has_else = true;
      break;
    }

    if ( symbol_name(cadar(c)) == "=>" )
      // produces: (<expression / procedure> <key>)
      exprs = list(cons(car(cdr(cdar(c))), cons(symbol("value", e))));

    cons_t *clause =
      cons(symbol("memv", e),
        cons(symbol("value", e),
          cons(cons(symbol("quote", e), cons(datum)))));

    clause = splice(cons(clause), exprs);
    clauses = append(clauses, cons(clause));
  }

  /*
  if ( !has_else ) {
    // insert unspecified return value
    clauses = append(clauses,
        cons(cons(symbol("else", e), cons(unspecified()))));
  }
  */

  cons_t *let = append(
    cons(symbol("let", e),
      cons(cons(cons(symbol("value", e), cons(key))))),
    cons(cons(symbol("cond", e), clauses)));

  return let;
}

cons_t* proc_gcd(cons_t* p, environment_t* e)
{
  switch ( length(p) ) {
  case 0:
    return integer(0);

  case 1:
    assert_type(INTEGER, car(p));
    return integer(abs(car(p)->integer));

  case 2: {
    assert_type(INTEGER, car(p));
    assert_type(INTEGER, cadr(p));

    int a = abs(car(p)->integer),
        b = abs(cadr(p)->integer);

    return integer(gcd(a, b));
  }

  default: {
    /*
     * We have at least 3 numbers; handle recursively, since
     * gcd(a, b, c) = gcd(gcd(a, b), c)
     */
    cons_t *r = car(p);
    p = cdr(p);

    while ( !nullp(p) ) {
      r = proc_gcd(list(r, car(p)), e);
      p = cdr(p);
    }

    return integer(r->integer);
  } }
}

static cons_t* proc_dummy_placeholder(cons_t*, environment_t*)
{
  return nil();
}

named_function_t exports_base[] = {
  {"*", proc_mul, false},
  {"+", proc_add, false},
  {"-", proc_sub, false},
  {"/", proc_divf, false},
  {"<", proc_less, false},
  {"<=", proc_lteq, false},
  {"=", proc_eqintp, false},
  {">", proc_greater, false},
  {">=", proc_gteq, false},
  {"abs", proc_abs, false},
  {"and", proc_and, false},
  {"append", proc_append, false},
  {"assoc", proc_assoc, false},
  {"assq", proc_assq, false},
  {"assv", proc_assv, false},
  {"binary-port?", proc_binary_portp, false},
  {"boolean->string", proc_boolean_to_string, false},
  {"boolean?", proc_booleanp, false},
  {"bytevector-copy!", proc_bytevector_copy_bang, false},
  {"bytevector-copy", proc_bytevector_copy, false},
  {"bytevector-copy-partial!", proc_bytevector_copy_partial_bang, false},
  {"bytevector-copy-partial", proc_bytevector_copy_partial, false},
  {"bytevector-length", proc_bytevector_length, false},
  {"bytevector-u8-ref", proc_bytevector_u8_ref, false},
  {"bytevector-u8-set!", proc_bytevector_u8_set_bang, false},
  {"bytevector?", proc_bytevectorp, false},
  {"car", proc_car, false},
  {"case", proc_case, false},
  {"cdr", proc_cdr, false},
  {"char->integer", proc_char_to_integer, false},
  {"char<=?", proc_char_ltep, false},
  {"char<?", proc_char_ltp, false},
  {"char=?", proc_char_eqp, false},
  {"char>=?", proc_char_gtep, false},
  {"char>?", proc_char_gtp, false},
  {"char?", proc_charp, false},
  {"close-port", proc_close_port, false},
  {"cons", proc_cons, false},
  {"current-error-port", proc_current_error_port, false},
  {"current-input-port", proc_current_input_port, false},
  {"current-output-port", proc_current_output_port, false},
  {"eq?", proc_eqp, false},
  {"equal?", proc_equalp, false},
  {"eqv?", proc_eqvp, false},
  {"error", proc_error, false},
  {"even?", proc_evenp, false},
  {"expt", proc_expt, false},
  {"file-exists?", proc_file_existsp, false},
  {"finite?", proc_finitep, false},
  {"gcd", proc_gcd, false},
  {"infinite?", proc_infinitep, false},
  {"input-port?", proc_input_portp, false},
  {"integer->char", proc_integer_to_char, false},
  {"integer?", proc_integerp, false},
  {"lcm", proc_lcm, false},
  {"length", proc_length, false},
  {"list", proc_list, false},
  {"list->string", proc_list_to_string, false},
  {"list->vector", proc_list_to_vector, false},
  {"list-ref", proc_list_ref, false},
  {"list-tail", proc_list_tail, false},
  {"list?", proc_listp, false},
  {"make-bytevector", proc_make_bytevector, false},
  {"make-string", proc_make_string, false},
  {"make-vector", proc_make_vector, false},
  {"map", proc_map, false},
  {"max", proc_max, false},
  {"member", proc_member, false},
  {"memq", proc_memq, false},
  {"memv", proc_memv, false},
  {"min", proc_min, false},
  {"modulo", proc_modulo, false},
  {"nan?", proc_nanp, false},
  {"negative?", proc_negativep, false},
  {"newline", proc_newline, false},
  {"not", proc_not, false},
  {"null?", proc_nullp, false},
  {"number->string", proc_number_to_string, false},
  {"number?", proc_numberp, false},
  {"odd?", proc_oddp, false},
  {"or", proc_or, false},
  {"output-port?", proc_output_portp, false},
  {"pair?", proc_pairp, false},
  {"port-open?", proc_port_openp, false},
  {"port?", proc_portp, false},
  {"positive?", proc_positivep, false},
  {"procedure?", proc_procedurep, false},
  {"real?", proc_realp, false},
  {"reverse", proc_reverse, false},
  {"round", proc_round, false},
  {"string", proc_string, false},
  {"string->list", proc_string_to_list, false},
  {"string->number", proc_string_to_number, false},
  {"string->symbol", proc_string_to_symbol, false},
  {"string->vector", proc_string_to_vector, false},
  {"string-append", proc_strcat, false},
  {"string-length", proc_string_length, false},
  {"string-ref", proc_string_ref, false},
  {"string<=?", proc_string_ltep, false},
  {"string<?", proc_string_ltp, false},
  {"string=?", proc_string_eqp, false},
  {"string>=?", proc_string_gtep, false},
  {"string>?", proc_string_gtp, false},
  {"string?", proc_stringp, false},
  {"substring", proc_substring, false},
  {"symbol->string", proc_symbol_to_string, false},
  {"symbol?", proc_symbolp, false},
  {"textual-port?", proc_textual_portp, false},
  {"truncate", proc_truncate, false},
  {"vector", proc_vector, false},
  {"vector->list", proc_vector_to_list, false},
  {"vector->string", proc_vector_to_string, false},
  {"vector-copy", proc_vector_copy, false},
  {"vector-fill!", proc_vector_fill, false},
  {"vector-length", proc_vector_length, false},
  {"vector-ref", proc_vector_ref, false},
  {"vector-set!", proc_vector_set, false},
  {"vector?", proc_vectorp, false},
  {"write", proc_write, false},
  {"xor", proc_xor, false},
  {"zero?", proc_zerop, false},
  /*
   * Following is the list of definitions recognized
   * by eval.  Since we now have syntactic closures,
   * we can remove them from eval() and invoke them
   * as ordinary syntactic functions. (TODO)
   */
  {"apply", proc_dummy_placeholder, true},
  {"begin", proc_dummy_placeholder, true},
  {"cond", proc_dummy_placeholder, true},
  {"define", proc_dummy_placeholder, true},
  {"define-syntax", proc_dummy_placeholder, true},
  {"do", proc_dummy_placeholder, true},
  {"eval", proc_dummy_placeholder, true},
  {"if", proc_dummy_placeholder, true},
  {"lambda", proc_dummy_placeholder, true},
  {"let", proc_dummy_placeholder, true},
  {"let*", proc_dummy_placeholder, true},
  {"letrec", proc_dummy_placeholder, true},
  {"quasiquote", proc_dummy_placeholder, true},
  {"quote", proc_dummy_placeholder, true},
  {"set!", proc_dummy_placeholder, true},
  {"set-car!", proc_dummy_placeholder, true},
  {"set-cdr!", proc_dummy_placeholder, true},
  {NULL, NULL, false} /* terminate with null */
};

}
