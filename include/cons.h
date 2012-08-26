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

typedef double decimal_t;

#ifndef INC_MICKEY_CONS_H
#define INC_MICKEY_CONS_H

#include <stdexcept>
#include <string>
#include <map>
#include <vector>

/*
 * Function signatures for all C procedures; they all take a cons_t* and an
 * environment and always returns cons_t*.
 */
typedef struct cons_t* (*lambda_t)(struct cons_t*, struct environment_t*);

/*
 * Symbol tables maps strings to values.
 */
typedef std::map<std::string, struct cons_t*> dict_t;


/*
 * Basic scheme types, plus some extended ones.
 */
enum type_t {
  NIL,
  BOOLEAN,
  CHAR,
  INTEGER,
  DECIMAL,
  CLOSURE,
  PAIR,
  SYMBOL,
  STRING,
  VECTOR,
  CONTINUATION,
  BYTEVECTOR,
  SYNTAX,
  PORT,
  ENVIRONMENT,
  POINTER
};

/*
 * Environment holds a symbol table and points to an optional outer (or
 * parent) environment.
 */
struct environment_t {
  struct environment_t *outer;
  dict_t symbols;

  environment_t* extend();
  struct cons_t* lookup(const std::string& name) const;
  struct cons_t* lookup_or_throw(const std::string& name) const;
  struct cons_t* define(const std::string& name, lambda_t func, bool syntactic = false);
  struct cons_t* define(const std::string& name, cons_t* body);
  environment_t* outermost();

private:
  environment_t() : outer(NULL)
  {
  }

  /*
   * Restrict instantiation to these functions:
   */
  friend environment_t* null_environment(int);
  friend environment_t* null_import_environment();

  /*
   * Prevent copy construction and assignment:
   */
  environment_t(const environment_t&);
  environment_t& operator=(const environment_t&);
};

/*
 * Continuations are not supported yet.
 */
struct continuation_t {
};

/*
 * A closure consists of a function and an environment.  A syntax flag tells
 * whether to evaluate parameters before function invocation.
 */
struct closure_t {
  lambda_t function;
  environment_t* environment;
  bool syntactic;
};

/*
 * Denotes a syntax transformer used to expand macros.
 */
struct syntax_t {
  cons_t* transformer;
  environment_t* environment;
};

/*
 * Arrays.
 */
struct vector_t {
  std::vector<cons_t*> vector;

  vector_t()
  {
  }

  vector_t(const vector_t& v)
  {
    if ( this != &v )
      vector = v.vector;
  }

  vector_t(size_t size) : vector(size)
  {
  }

  vector_t(size_t size, cons_t* fill) : vector(size, fill)
  {
  }
};

/*
 * Array of unsigned 8-bit integer values.
 */
struct bytevector_t {
  std::vector<uint8_t> bytevector;

  bytevector_t()
  {
  }

  bytevector_t(const bytevector_t& v) : bytevector(v.bytevector)
  {
  }

  bytevector_t(size_t size) : bytevector(size)
  {
  }

  bytevector_t(size_t size, const uint8_t fill) : bytevector(size, fill)
  {
  }

  bytevector_t(const std::vector<uint8_t>& v) : bytevector(v)
  {
  }
};

/*
 * A symbol.
 */
class symbol_t {
public:
  const std::string *n;

  symbol_t() : n(NULL)
  {
  }

  symbol_t(const symbol_t& s) : n(s.n)
  {
  }

  const std::string& name() const
  {
    return *n;
  }
};

/*
 * Symbols are uniquely identified by their names, so all symbols with equal
 * names will actually point to the same memory location.
 */
const symbol_t* create_symbol(const std::string& s);

/*
 * Port types
 */
enum porttype_t {
  TEXTUAL_PORT,
  BINARY_PORT
};

/*
 * An input or output port to either files or strings.
 */
class port_t
{
  FILE *f;
  char *s;
public:
  porttype_t port_type;
  bool writable, readable;

  port_t() :
    f(NULL),
    s(NULL),
    port_type(TEXTUAL_PORT),
    writable(false),
    readable(false)
  {
  }

  port_t(FILE* file) :
    f(file),
    s(NULL),
    port_type(TEXTUAL_PORT),
    writable(false),
    readable(false)
  {
  }

  port_t(const port_t& r)
  {
    if ( this != &r )
      memcpy(this, &r, sizeof(port_t));
  }

  port_t& textual()
  {
    port_type = TEXTUAL_PORT;
    return *this;
  }

  port_t& input()
  {
    readable = true;
    return *this;
  }

  port_t& output()
  {
    writable = true;
    return *this;
  }

  bool fileport() const
  {
    return f!=NULL && s==NULL;
  }

  bool stringport() const
  {
    return f==NULL && s!=NULL;
  }

  FILE* file() const
  {
    return f;
  }

  bool isopen() const
  {
    // file port
    if ( f != NULL )
      return ftell(f) != -1; // TODO: Is there a better way to check?

    // string port
    if ( s != NULL )
      return writable || readable;

    return false;
  }

  bool iswritable() const
  {
    return writable;
  }

  bool isreadable() const
  {
    return readable;
  }

  bool istextual() const
  {
    return port_type == TEXTUAL_PORT;
  }

  bool isbinary() const
  {
    return port_type == BINARY_PORT;
  }

  friend bool operator==(const port_t& l, const port_t& r)
  {
    return l.f == r.f && l.port_type == r.port_type &&
      l.writable == r.writable && l.readable == r.readable &&
      l.s == r.s;
  }

  port_t& operator=(const port_t& r)
  {
    if ( this != &r )
      memcpy(this, &r, sizeof(port_t));
    return *this;
  }

  void close()
  {
    if ( f != NULL ) {
      fclose(f);
      readable = writable = false;
    }

    if ( s != NULL ) {
      s = NULL;
      readable = writable = false;
    }
  }
};

/*
 * A tagged void pointer.  Used for hairy stuff like dynamic loading (hairy
 * because we don't normally want to expose this to the scheme environment,
 * and because converting function pointers to and from void pointers is not
 * guaranteed by the C standard to work).
 */
struct pointer_t {
  const char* tag;
  void* value;

  pointer_t(const char* tag_, void* ptr)
    : tag(tag_), value(ptr)
  {
  }

  friend bool operator==(const pointer_t& l, const pointer_t& r)
  {
    return l.tag == r.tag && l.value == r.value;
  }
};

/*
 * A variant variable.  Called a cons-cell, but that's incorrect.
 *
 * TODO: To cons_t, Add `marked` (for GC) and `mutable/immutable` (per spec)
 */
struct cons_t {
  type_t type;
  union {
    bool boolean;
    char character;
    int integer;
    decimal_t decimal;
    struct { cons_t *car, *cdr; }; // pair
    closure_t* closure;
    syntax_t* syntax;
    const symbol_t* symbol;
    const char* string;
    vector_t* vector;
    bytevector_t* bytevector;
    continuation_t* continuation;
    port_t* port;
    environment_t *environment; // first-class environments
    pointer_t *pointer;
  };
};

/*
 * Merges the two environments by copying `b´ into `a´.
 * Returns number of symbols copied.
 */
int merge(environment_t *to, const environment_t *from);

#endif
