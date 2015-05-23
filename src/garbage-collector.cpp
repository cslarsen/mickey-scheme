/*
 * Mickey Scheme
 *
 * Copyright (C) 2015 Christian Stigen Larsen <csl@csl.name>
 * http://csl.name>
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include <map>
#include <vector>
#include "mickey/mickey.h"

static std::vector<const cons_t*> global_roots;

// TODO: Use tri-coloring
enum gc_state {
  GC_UNREACHABLE,
  GC_REACHABLE
};

typedef std::map<const bytevector_t*, gc_state> bytevector_map;
typedef std::map<const char**, gc_state> strings_map;
typedef std::map<const char*, gc_state> string_map;
typedef std::map<const closure_t*, gc_state> closure_map;
typedef std::map<const cons_t*, gc_state> cons_map;
typedef std::map<const continuation_t*, gc_state> continuation_map;
typedef std::map<const dict_t*, gc_state> dict_map;
typedef std::map<const environment_t*, gc_state> environment_map;
typedef std::map<const library_t*, gc_state> library_map;
typedef std::map<const pointer_t*, gc_state> pointer_map;
typedef std::map<const port_t*, gc_state> port_map;
typedef std::map<const program_t*, gc_state> program_map;
typedef std::map<const symbol_t, gc_state> symbol_map;
typedef std::map<const syntax_t*, gc_state> syntax_map;
typedef std::map<const vector_t*, gc_state> vector_map;

class gc_storage {
  bytevector_map bytevectors;
  closure_map closures;
  cons_map conses;
  continuation_map continuations;
  dict_map dicts;
  environment_map environments;
  library_map libraries;
  pointer_map pointers;
  port_map ports;
  program_map programs;
  string_map strings;
  strings_map pstrings;
  symbol_map symbols;
  syntax_map syntaxes;
  vector_map vectors;

  void mark_by_type(const cons_t* p);
  size_t sweep();

  template<typename E>
  size_t sweep_ptr(typename std::map<E, gc_state>& s)
  {
    size_t reclaimed = 0;

    for ( typename std::map<E, gc_state>::iterator kv = s.begin();
          kv != s.end(); )
    {
      E ptr = kv->first;
      gc_state& state = kv->second;

      switch ( state ) {
        case GC_UNREACHABLE:
          // reclaim memory and continue
          delete(ptr);
          kv = s.erase(kv);
          ++reclaimed;
          break;

        case GC_REACHABLE:
          // clear flag and continue
          state = GC_UNREACHABLE;
          ++kv;
          break;
      }
    }

    return reclaimed;
  }

  size_t sweep_symbols()
  {
    size_t reclaimed = 0;

    for ( symbol_map::iterator kv = symbols.begin(); kv != symbols.end(); ) {
      gc_state& state = kv->second;

      switch ( state ) {
        case GC_UNREACHABLE:
          // reclaim memory and continue
          kv = symbols.erase(kv);
          ++reclaimed;
          break;

        case GC_REACHABLE:
          // clear flag and continue
          state = GC_UNREACHABLE;
          ++kv;
          break;
      }
    }

    return reclaimed;
  }

public:
  size_t size() const;
  size_t collect(const cons_t* roots);
  size_t collect();

  cons_t* alloc_cons(const type_t& type);
  dict_t* alloc_dict();
  const symbol_t* alloc_symbol(const char* name);
  port_t* alloc_port(FILE* f);
  environment_t* alloc_environment();
  pointer_t* alloc_pointer(const char* tag, void* ptr);
  closure_t* alloc_closure();
  syntax_t* alloc_syntax(cons_t* transformer = NULL, environment_t* env = NULL);

  vector_t* alloc_vector();
  vector_t* alloc_vector(const vector_t* v);
  vector_t* alloc_vector(const size_t& size);
  vector_t* alloc_vector(const size_t& size, cons_t* fill);

  bytevector_t* alloc_bytevector();
  bytevector_t* alloc_bytevector(const bytevector_t* v);
  bytevector_t* alloc_bytevector(const size_t& size);
  bytevector_t* alloc_bytevector(const size_t& size, const uint8_t fill);
  bytevector_t* alloc_bytevector(const std::vector<uint8_t>& v);

  program_t* alloc_program();
  library_t* alloc_library();

  char* alloc_string(const size_t length);
  char* alloc_string(const char* source);

  void mark(const bytevector_t* p);
  void mark(const char* p);
  void mark(const char** p);
  void mark(const closure_t* p);
  void mark(const cons_t* p);
  void mark(const continuation_t* p);
  void mark(const dict_t* p);
  void mark(const environment_t* p);
  void mark(const library_t* p);
  void mark(const pointer_t* p);
  void mark(const port_t* p);
  void mark(const program_t* p);
  void mark(const symbol_t* p);
  void mark(const syntax_t* p);
  void mark(const vector_t* p);
};

void gc_storage::mark_by_type(const cons_t* p)
{
  if ( !p )
    return;

  // CLOSURE, SYNTAX
  switch ( p->type ) {
    case BYTEVECTOR: mark(p->bytevector); break;
    case CLOSURE: mark(p->closure); break;
    case CONTINUATION: mark(p->continuation); break;
    case ENVIRONMENT: mark(p->environment); break;
    case PAIR: mark(p->car); mark(p->cdr); break;
    case POINTER: mark(p->pointer); break;
    case PORT: mark(p->port); break;
    case STRING: mark(p->string); break;
    case SYMBOL: mark(p->symbol); break;
    case SYNTAX: mark(p->syntax); break;
    case VECTOR: mark(p->vector); break;
    default: break;
  }
}

size_t gc_storage::collect(const cons_t* root)
{
  mark(root);
  return sweep();
}

size_t gc_storage::collect()
{
  for ( size_t n=0; n < global_roots.size(); ++n )
    mark(global_roots[n]);
  return sweep();
}

size_t gc_storage::size() const
{
  return 0
    + bytevectors.size()
    + closures.size()
    + conses.size()
    + continuations.size()
    + dicts.size()
    + environments.size()
    + libraries.size()
    + pointers.size()
    + ports.size()
    + programs.size()
    + pstrings.size()
    + strings.size()
    + symbols.size()
    + syntaxes.size()
    + vectors.size()
  ;
}

size_t gc_storage::sweep()
{
  return 0
       + sweep_ptr(bytevectors)
       + sweep_ptr(closures)
       + sweep_ptr(conses)
       + sweep_ptr(continuations)
       + sweep_ptr(dicts)
       + sweep_ptr(environments)
       + sweep_ptr(libraries)
       + sweep_ptr(pointers)
       + sweep_ptr(ports)
       + sweep_ptr(programs)
       + sweep_ptr(pstrings)
       + sweep_ptr(strings)
       + sweep_ptr(syntaxes)
       + sweep_ptr(vectors)
       + sweep_symbols()
  ;
}

cons_t* gc_storage::alloc_cons(const type_t& type)
{
  cons_t* p = new cons_t();
  p->type = type;
  conses[p] = GC_UNREACHABLE;
  return p;
}

dict_t* gc_storage::alloc_dict()
{
  dict_t* p = new dict_t();
  dicts[p] = GC_UNREACHABLE;
  return p;
}

closure_t* gc_storage::alloc_closure()
{
  closure_t* p = new closure_t();
  closures[p] = GC_UNREACHABLE;
  return p;
}

const symbol_t* gc_storage::alloc_symbol(const char* name)
{
  // TODO: Check for symbol validity
  //       (e.g., cannot begin with a number, etc).

  const symbol_t* s = new symbol_t(name);

  // Does symbol already exist?
  symbol_map::iterator kv = symbols.find(*s);

  if ( kv != symbols.end() ) {
    // Yes, reuse the existing one
    delete(s);
    s = &(kv->first);
  } else {
    // No, store the new one
    symbols[*s] = GC_UNREACHABLE;
  }

  return s;
}

port_t* gc_storage::alloc_port(FILE* f)
{
  port_t* p = new port_t(f);
  ports[p] = GC_UNREACHABLE;
  return p;
}

environment_t* gc_storage::alloc_environment()
{
  environment_t* p = new environment_t();
  environments[p] = GC_UNREACHABLE;
  return p;
}

pointer_t* gc_storage::alloc_pointer(const char* tag, void* ptr)
{
  pointer_t* p = new pointer_t(tag, ptr);
  pointers[p] = GC_UNREACHABLE;
  return p;
}

syntax_t* gc_storage::alloc_syntax(cons_t* transformer, environment_t* env)
{
  syntax_t* p = new syntax_t();
  p->transformer = transformer;
  p->environment = env;
  syntaxes[p] = GC_UNREACHABLE;
  return p;
}

vector_t* gc_storage::alloc_vector()
{
  vector_t* p = new vector_t();
  vectors[p] = GC_UNREACHABLE;
  return p;
}

vector_t* gc_storage::alloc_vector(const vector_t* v)
{
  vector_t* p = new vector_t(*v);
  vectors[p] = GC_UNREACHABLE;
  return p;
}

vector_t* gc_storage::alloc_vector(const size_t& size)
{
  vector_t* p = new vector_t(size);
  vectors[p] = GC_UNREACHABLE;
  return p;
}

vector_t* gc_storage::alloc_vector(const size_t& size, cons_t* fill)
{
  vector_t* p = new vector_t(size, fill);
  vectors[p] = GC_UNREACHABLE;
  return p;
}

bytevector_t* gc_storage::alloc_bytevector()
{
  bytevector_t* p = new bytevector_t();
  bytevectors[p] = GC_UNREACHABLE;
  return p;
}

bytevector_t* gc_storage::alloc_bytevector(const bytevector_t* v)
{
  bytevector_t* p = new bytevector_t(*v);
  bytevectors[p] = GC_UNREACHABLE;
  return p;
}

bytevector_t* gc_storage::alloc_bytevector(const size_t& size)
{
  bytevector_t* p = new bytevector_t(size);
  bytevectors[p] = GC_UNREACHABLE;
  return p;
}

bytevector_t* gc_storage::alloc_bytevector(const size_t& size, const uint8_t fill)
{
  bytevector_t* p = new bytevector_t(size, fill);
  bytevectors[p] = GC_UNREACHABLE;
  return p;
}

bytevector_t* gc_storage::alloc_bytevector(const std::vector<uint8_t>& v)
{
  bytevector_t* p = new bytevector_t(v);
  bytevectors[p] = GC_UNREACHABLE;
  return p;
}

program_t* gc_storage::alloc_program()
{
  program_t* p = new program_t();
  programs[p] = GC_UNREACHABLE;
  return p;
}

library_t* gc_storage::alloc_library()
{
  library_t* p = new library_t();
  libraries[p] = GC_UNREACHABLE;
  return p;
}

char* gc_storage::alloc_string(const size_t length)
{
  char* p = new char[1+length];
  memset(p, 0, 1+length);
  strings[p] = GC_UNREACHABLE;
  return p;
}

char* gc_storage::alloc_string(const char* source)
{
  char* p = new char[1+strlen(source? source : "")];
  strcpy(p, source);
  strings[p] = GC_UNREACHABLE;
  return p;
}

void gc_storage::mark(const closure_t* p)
{
  if ( p ) {
    closure_map::iterator kv = closures.find(p);

    if ( kv != closures.end() ) {
      if ( kv->second == GC_UNREACHABLE ) {
        kv->second = GC_REACHABLE;
        mark(p->environment);
        mark(p->body);
        mark(p->args);
      }
    } else
      printf("gc error: closure_t* %p not in root set.\n", p);
  }
}

void gc_storage::mark(const program_t* p)
{
  if ( p ) {
    program_map::iterator kv = programs.find(p);

    if ( kv != programs.end() ) {
      if ( kv->second == GC_UNREACHABLE ) {
        kv->second = GC_REACHABLE;
        mark(p->root);
        mark(p->globals);
      }
    } else
      printf("gc error: symbol_t* %p not in root set.\n", p);
  }
}

void gc_storage::mark(const library_t* p)
{
  if ( p ) {
    library_map::iterator kv = libraries.find(p);

    if ( kv != libraries.end() ) {
      if ( kv->second == GC_UNREACHABLE ) {
        kv->second = GC_REACHABLE;
        mark(p->name);
        mark(p->exports);
        mark(p->internals);
      }
    } else
      printf("gc error: symbol_t* %p not in root set.\n", p);
  }
}

void gc_storage::mark(const symbol_t* p)
{
  if ( p ) {
    symbol_map::iterator kv = symbols.find(*p);

    if ( kv != symbols.end() ) {
      if ( kv->second == GC_UNREACHABLE ) {
        kv->second = GC_REACHABLE;
      }
    } else
      printf("gc error: symbol_t* %p not in root set.\n", p);
  }
}

void gc_storage::mark(const syntax_t* p)
{
  if ( p ) {
    syntax_map::iterator kv = syntaxes.find(p);

    if ( kv != syntaxes.end() ) {
      if ( kv->second == GC_UNREACHABLE ) {
        kv->second = GC_REACHABLE;
        mark(p->transformer);
        mark(p->environment);
      }
    } else
      printf("gc error: syntax_t* %p not in root set.\n", p);
  }
}

void gc_storage::mark(const cons_t* p)
{
  if ( p ) {
    cons_map::iterator kv = conses.find(p);

    if ( kv != conses.end() ) {
      if ( kv->second == GC_UNREACHABLE ) {
        kv->second = GC_REACHABLE;
        mark_by_type(p);
      }
    } else
      printf("gc error: cons_t* %p not in root set.\n", p);
  }
}

void gc_storage::mark(const pointer_t* p)
{
  if ( p ) {
    pointer_map::iterator kv = pointers.find(p);

    if ( kv != pointers.end() )
      kv->second = GC_REACHABLE;
    else
      printf("gc error: pointer_t* tag '%s' %p not in root set.\n", p->tag,
          p);
  }
}

void gc_storage::mark(const environment_t* p)
{
  if ( p ) {
    environment_map::iterator kv = environments.find(p);

    if ( kv != environments.end() ) {
      if ( kv->second == GC_UNREACHABLE ) {
        kv->second = GC_REACHABLE;
        mark(p->symbols);
        mark(p->outer);
      }
    } else
      printf("gc error: environment_t* %p not in root set.\n", p);
  }
}

void gc_storage::mark(const vector_t* p)
{
  if ( p ) {
    vector_map::iterator kv = vectors.find(p);

    if ( kv != vectors.end() ) {
      if ( kv->second == GC_UNREACHABLE ) {
        kv->second = GC_REACHABLE;
        for ( std::vector<cons_t*>::const_iterator i = p->vector.begin();
              i != p->vector.end(); ++i)
        {
          mark(*i);
        }
      }
    } else
      printf("gc error: vector_t* %zu items %p not in root set.\n",
          p->vector.size(), p);
  }
}

void gc_storage::mark(const bytevector_t* p)
{
  if ( p ) {
    bytevector_map::iterator kv = bytevectors.find(p);

    if ( kv != bytevectors.end() )
      kv->second = GC_REACHABLE;
    else
      printf("gc error: bytevector_t* %zu items %p not in root set.\n",
          p->bytevector.size(), p);
  }
}

void gc_storage::mark(const char* p)
{
  if ( p ) {
    string_map::iterator kv = strings.find(p);

    if ( kv != strings.end() )
      kv->second = GC_REACHABLE;
      // TODO: Check char allocator storage
    else
      printf("gc error: char* length %zu %p not in root set.\n", strlen(p),
          p);
  }
}

void gc_storage::mark(const char** p)
{
  if ( p ) {
    strings_map::iterator kv = pstrings.find(p);

    if ( kv != pstrings.end() ) {
      if ( kv->second == GC_UNREACHABLE ) {
        kv->second = GC_REACHABLE;
        for ( const char** pc = p; *pc; ++pc )
          mark(*pc);
      }
    } else
      printf("gc error: char** %p not in root set.\n", p);
  }
}

void gc_storage::mark(const dict_t* p)
{
  if ( p ) {
    dict_map::iterator kv = dicts.find(p);

    if ( kv != dicts.end() ) {
      if ( kv->second == GC_UNREACHABLE ) {
        kv->second = GC_REACHABLE;
        for ( dict_t::const_iterator i = p->begin(); i != p->end(); ++i )
          mark(i->second);
      }
    } else
      printf("gc error: dict_t* %p not in root set.\n", p);
  }
}

void gc_storage::mark(const continuation_t* p)
{
  if ( p ) {
    continuation_map::iterator kv = continuations.find(p);

    if ( kv != continuations.end() ) {
      if ( kv->second == GC_UNREACHABLE ) {
        kv->second = GC_REACHABLE;
      }
    } else
      printf("gc error: continuation_t* %p not in root set.\n", p);
  }
}

void gc_storage::mark(const port_t* p)
{
  if ( p ) {
    port_map::iterator kv = ports.find(p);

    if ( kv != ports.end() ) {
      if ( kv->second == GC_UNREACHABLE ) {
        kv->second = GC_REACHABLE;
      }
    } else
      printf("gc error: port_t* %p not in root set.\n", p);
  }
}

/*****************************************************************************/

static gc_storage gc;

size_t gc_status()
{
  return gc.size();
}

size_t gc_collect()
{
  return gc.collect();
}

size_t gc_collect(cons_t* roots)
{
  return gc.collect(roots);
}

size_t gc_collect(program_t* p)
{
  return gc.collect(cons(p->root, environment(p->globals)));
}

cons_t* gc_alloc_cons()
{
  return gc.alloc_cons(NIL);
}

cons_t* gc_alloc_cons(const type_t& type)
{
  return gc.alloc_cons(type);
}

const symbol_t* gc_alloc_symbol(const char* name)
{
  return gc.alloc_symbol(name);
}

pointer_t* gc_alloc_pointer(const char* tag, void* ptr)
{
  return gc.alloc_pointer(tag, ptr);
}

program_t* gc_alloc_program()
{
  return gc.alloc_program();
}

library_t* gc_alloc_library()
{
  return gc.alloc_library();
}

closure_t* gc_alloc_closure()
{
  return gc.alloc_closure();
}

syntax_t* gc_alloc_syntax(cons_t* transformer,
                          environment_t* env)
{
  return gc.alloc_syntax(transformer, env);
}

environment_t* gc_alloc_environment()
{
  return gc.alloc_environment();
}

vector_t* gc_alloc_vector()
{
  return gc.alloc_vector();
}

vector_t* gc_alloc_vector(const vector_t* v)
{
  return gc.alloc_vector(v);
}

vector_t* gc_alloc_vector(const size_t& size)
{
  return gc.alloc_vector(size);
}

vector_t* gc_alloc_vector(const size_t& size, cons_t* fill)
{
  return gc.alloc_vector(size, fill);
}

bytevector_t* gc_alloc_bytevector()
{
  return gc.alloc_bytevector();
}

bytevector_t* gc_alloc_bytevector(const bytevector_t* v)
{
  return gc.alloc_bytevector(v);
}

bytevector_t* gc_alloc_bytevector(const size_t& size)
{
  return gc.alloc_bytevector(size);
}

bytevector_t* gc_alloc_bytevector(const size_t& size, const uint8_t fill)
{
  return gc.alloc_bytevector(size, fill);
}

bytevector_t* gc_alloc_bytevector(const std::vector<uint8_t>& v)
{
  return gc.alloc_bytevector(v);
}

char* gc_alloc_string(const char* source)
{
  return gc.alloc_string(source);
}

char* gc_alloc_string(const size_t length)
{
  return gc.alloc_string(length);
}

dict_t* gc_alloc_dict()
{
  return gc.alloc_dict();
}

void gc_add_root(const cons_t* p)
{
  global_roots.push_back(p);
}

void gc_add_root(environment_t* e)
{
  global_roots.push_back(environment(e));
}

void gc_add_root(program_t* p)
{
  global_roots.push_back(cons(p->root, environment(p->globals)));
}
