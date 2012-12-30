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
#include <ctype.h>
#include <cmath>
#include "library/scheme-base.h"
#include "system-features.h"

extern "C" {

cons_t* proc_base_char_foldcase(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(CHAR, car(p));
  return character(tolower(car(p)->character));
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

  return unspecified();
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

  size_t len = car(p)->number.integer;
  char   ch  = length(p)==1? ' ' : cadr(p)->character;

  return string(std::string(len, ch).c_str());
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

  if ( name->symbol->empty() )
    raise(runtime_exception("Cannot define with empty variable name")); // TODO: Even possible?

  env->define(*name->symbol, body);
  return unspecified();
}

cons_t* proc_define_syntax(cons_t *p, environment_t *env)
{
  // (define <name> <body>)
  assert_type(SYMBOL, car(p));
  cons_t *name = car(p);
  cons_t *syntax = cadr(p);

  if ( name->symbol->empty() )
    raise(runtime_exception("Cannot define-syntax with empty name"));

  env->define(*name->symbol, syntax);
  return unspecified();
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

cons_t* proc_close_port(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PORT, car(p));
  car(p)->port->close();
  return unspecified();
}

cons_t* proc_close_input_port(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PORT, car(p));

  if ( !car(p)->port->isreadable() )
    raise(runtime_exception("Can only close input ports"));

  /*
   * TODO: Update port_t to support both input and output,
   *       or update it to support sockets and then see
   *       how it should be done.
   */
  car(p)->port->close();
  return unspecified();
}

cons_t* proc_close_output_port(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PORT, car(p));

  if ( !car(p)->port->iswritable() )
    raise(runtime_exception("Can only close output ports"));

  /*
   * TODO: Update port_t to support both input and output,
   *       or update it to support sockets and then see
   *       how it should be done.
   */
  car(p)->port->close();
  return unspecified();
}

cons_t* proc_vector(cons_t* p, environment_t*)
{
  return vector(p);
}

cons_t* proc_bytevector(cons_t* p, environment_t*)
{
  return bytevector(p);
}

cons_t* proc_make_vector(cons_t* p, environment_t*)
{
  assert_length(p, 1, 2);
  assert_type(INTEGER, car(p));

  size_t k = car(p)->number.integer;
  cons_t *fill = length(p)>1? cadr(p) : NULL;

  return !fill? vector(p, k) : vector(p, k, fill);
}

cons_t* proc_make_bytevector(cons_t* p, environment_t*)
{
  assert_length(p, 1, 2);
  assert_type(INTEGER, car(p));

  size_t k = car(p)->number.integer;
  cons_t *fill = length(p)>1? cadr(p) : NULL;

  if ( !fill ) {
    return bytevector(k);
  } else {
    uint8_t u8fill = 0;
    switch ( type_of(fill) ) {
    case BOOLEAN: u8fill = fill->boolean; break;
    case CHAR:    u8fill = fill->character; break;
    case INTEGER: {
      if ( fill->number.integer < 0 || fill->number.integer > 255 )
        raise(runtime_exception(
          "make-vector only accepts unsigned 8-bit bytes"));
      u8fill = static_cast<uint8_t>(fill->number.integer);
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
  int ref = cadr(p)->number.integer;

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
  int ref = cadr(p)->number.integer;
  cons_t *set = caddr(p);

  if ( ref<0 || static_cast<size_t>(ref) >= v->vector.size() )
    raise(runtime_exception("Vector index out of range: " + to_s(ref)));

  v->vector[ref] = set;
  return unspecified();
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
  int k = cadr(p)->number.integer;

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

  int start = cadr(p)->number.integer;
  int end = caddr(p)->number.integer;

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

  int start = cadr(p)->number.integer;
  int end   = caddr(p)->number.integer;
  int at    = caddddr(p)->number.integer;

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

  return unspecified();
}

cons_t* proc_bytevector_u8_set_bang(cons_t* p, environment_t*)
{
  assert_length(p, 3);
  assert_type(BYTEVECTOR, car(p));
  assert_type(INTEGER, cadr(p));
  assert_type(INTEGER, caddr(p));

  bytevector_t *v = car(p)->bytevector;
  int k = cadr(p)->number.integer;
  int val = caddr(p)->number.integer;

  if ( k<0 || static_cast<size_t>(k) >= v->bytevector.size() )
    raise(runtime_exception("bytevector-u8-set! index out of range"));

  if ( val<0 || val>255 )
    raise(runtime_exception("bytevector-u8-set! byte value out of range"));

  v->bytevector[k] = static_cast<uint8_t>(val);
  return unspecified();
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
  return unspecified();
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

  return unspecified();
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

cons_t* proc_length(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(PAIR, car(p));
  assert_noncyclic(car(p));
  return integer(static_cast<int>(length(car(p))));
}

cons_t* proc_not(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(not_p(p));
}

cons_t* proc_xor(cons_t* p, environment_t*)
{
  return boolean(xor_p(p));
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
    return cons(cons(symbol("lambda"),
           cons(names, cons(proc_begin(body, e)))), values);

  /*
   * Transform named let to letrec.
   */
  return cons(symbol("letrec"),            // (letrec
           cons(cons(cons(name,               //   ((<name>
             cons(cons(symbol("lambda"),   //      (lambda
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
    inner = cons(cons(symbol("lambda"),
      cons(cons(car(names)), cons(inner))), cons(car(values)));

     names = cdr(names);
    values = cdr(values);
  }

  return inner;
}

cons_t* proc_letrec(cons_t* p, environment_t*)
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
  r = cons(symbol("let"), list(r));

  // (set! name-N value-N)
  for ( cons_t *n=names,
               *v=values;
        !nullp(n) && !nullp(v);
        n=cdr(n), v=cdr(v) )
  {
    r = append(r, cons(cons(symbol("set!"),
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
static cons_t* let(cons_t *bindings, cons_t *body, environment_t*)
{
  return cons(symbol("let"), cons(bindings, cons(body)));
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
    action = cons(cadr(cdar(p)), cons(symbol("result")));

  cons_t *otherwise = proc_cond(p, e);

  return (symbol_name(test) == "else")? action :
    let(list(list(symbol("result"), test)), // (let ((result <test>))
      cons(symbol("if"),                    //   (if result
        cons(symbol("result"),              //     (begin <action>)
          cons(cons(symbol("begin"),        //       <otherwise>))
            cons(action)),
              cons(otherwise)))), e);
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

cons_t* proc_begin(cons_t* p, environment_t*)
{
  return cons(symbol("begin"), p);
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
  return unspecified();
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
    fprintf(po->file(), !supports_feature("win32")? "\n" : "\r\n");

    /*
     * Can just as well flush it when we have
     * a newline (even though it slows everything
     * down, it's kinda neat to flush at these
     * points.
     */
    fflush(po->file());
  } else
    raise(runtime_exception("String ports are not supported: " + to_s(po)));

  return unspecified();
}

cons_t* proc_char_to_integer(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(CHAR, car(p));
  return integer(static_cast<int>(car(p)->character));
}

cons_t* proc_integer_to_char(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));
  return character(static_cast<char>(car(p)->number.integer));
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

  size_t n = cadr(p)->number.integer;
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

cons_t* proc_list_set(cons_t* p, environment_t*)
{
  assert_length(p, 3);
  assert_type(PAIR, car(p));
  assert_type(INTEGER, cadr(p));

  cons_t *l = car(p);
  size_t k = cadr(p)->number.integer;
  cons_t *obj = caddr(p);

  if ( cadr(p)->number.integer < 0 || k >= length(l) )
    raise(runtime_exception(format(
      "Invalid list index: %d", cadr(p)->number.integer)));

  // skip down to location
  for ( size_t n=0; n<k; ++n )
    l = cdr(l);

  // insert new item
  l->car = obj;
  return unspecified();
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

cons_t* proc_string_to_list(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(STRING, car(p));

  cons_t *r = list(NULL);

  for ( const char* s = car(p)->string; *s; ++s )
    r = append(r, list(character(*s)));

  return r;
}

cons_t* proc_string_to_symbol(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(STRING, car(p));
  return symbol(car(p)->string);
}

cons_t* proc_string_to_number(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(STRING, car(p));

  const char *s = car(p)->string;

  if ( isreal(s) )
    return real(to_f(s));
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

  char s[2];
  s[0] = car(p)->character;
  s[1] = '\0';

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
      substr(start->number.integer,
             len->number.integer).c_str());
}

cons_t* proc_string_ref(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_type(STRING, car(p));
  assert_type(INTEGER, cadr(p));

  if ( static_cast<size_t>(cadr(p)->number.integer) >= strlen(car(p)->string) )
    raise(runtime_exception("string-ref argument out of bounds"));

  return character(car(p)->string[ cadr(p)->number.integer]);
}

cons_t* proc_do(cons_t* p, environment_t*)
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
         *test_body = cons(symbol("begin"), cdaddr(p)),
         *body = cons(symbol("begin"), cdddr(p));

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
  cons_t* loop = cons(symbol("loop"));
  for ( cons_t *n = step; !nullp(n); n = cdr(n) )
    loop = append(loop, cons(car(n)));

  // (begin <body> (loop ...))
  body = append(body, cons(loop));

  // (if <test> <test_body> (begin <body> (loop ...))
  cons_t *if_expr =
    cons(symbol("if"),
      cons(test,
        cons(test_body,
          cons(body))));

  // (lambda (<var1> <var2> <varN>) <if_expr>)
  cons_t *lambda =
    cons(symbol("lambda"),
      cons(names,
        cons(if_expr)));

  // (loop <lambda>)
  loop =
    cons(symbol("loop"),
      cons(lambda));

  // (loop <init1> <init2> <initN>)
  cons_t *loop_init =
    cons(symbol("loop"),
      init);

  // (letrec ((loop <loop_body)) (loop <init1> ...))
  cons_t *letrec =
    cons(symbol("letrec"),
      cons(list(loop),
       cons(loop_init)));

  return letrec;
}

cons_t* proc_case(cons_t *p, environment_t*)
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

  cons_t *key = cadr(p);
  cons_t *clauses = cons(NULL);

  for ( cons_t *c = cddr(p); !nullp(c); c = cdr(c) ) {
    cons_t *datum = caar(c);
    cons_t *exprs = cdar(c);

    if ( symbol_name(datum) == "else" ) {
      if ( symbol_name(cadar(c)) == "=>" ) {
        // produces: (else (<expression / procedure> <key>))
        clauses = append(clauses,
                      cons(cons(symbol("else"),
                        cons(cons(cadr(cdar(c)),
                          cons(symbol("value")))))));
      } else
        clauses = append(clauses, c);

      break;
    }

    if ( symbol_name(cadar(c)) == "=>" )
      // produces: (<expression / procedure> <key>)
      exprs = list(cons(car(cdr(cdar(c))), cons(symbol("value"))));

    cons_t *clause =
      cons(symbol("memv"),
        cons(symbol("value"),
          cons(cons(symbol("quote"), cons(datum)))));

    clause = splice(cons(clause), exprs);
    clauses = append(clauses, cons(clause));
  }

  cons_t *let = append(
    cons(symbol("let"),
      cons(cons(cons(symbol("value"), cons(key))))),
    cons(cons(symbol("cond"), clauses)));

  return let;
}

cons_t* proc_read_line(cons_t* p, environment_t*)
{
  assert_length(p, 0, 1);

  FILE *f = global_opts.current_input_device.file();

  if ( length(p) == 1 ) {
    assert_type(PORT, car(p));
    port_t* po = car(p)->port;
    f = po->file();

    if ( !po->isreadable() )
      raise(runtime_exception("Output port is not writable"));

    if ( !po->fileport() )
      raise(runtime_exception("Only file ports are supported currently"));
  }

  std::string r;

  for ( int ch; (ch = fgetc(f)) != EOF && ch != '\n'; )
    r += static_cast<char>(ch);

  return string(r.c_str());
}

cons_t* proc_eof_object(cons_t* p, environment_t*)
{
  assert_length(p, 0);
  return pointer(new pointer_t("eof-object", NULL));
}

cons_t* proc_peek_char(cons_t* p, environment_t*)
{
  assert_length(p, 0, 1);

  FILE *f = global_opts.current_input_device.file();

  if ( length(p) == 1 ) {
    assert_type(PORT, car(p));
    port_t* po = car(p)->port;
    f = po->file();

    if ( !po->isreadable() )
      raise(runtime_exception("Output port is not writable"));

    if ( !po->fileport() )
      raise(runtime_exception("Only file ports are supported currently"));
  }

  /*
   * Peek
   */
  int ch = fgetc(f);

  if ( ch == EOF )
    return proc_eof_object(nil(), NULL);

  ungetc(ch, f);
  return character(static_cast<char>(ch));
}

cons_t* proc_dummy_placeholder(cons_t*, environment_t*)
{
  raise(general_exception(
    "You should never call proc_dummy_placeholder in "
    "scheme-base.cpp directly"));
  return unspecified();
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
               apply(proc->closure, args, proc->closure->environment)));
  }

  return result;
}

} // extern "C"
