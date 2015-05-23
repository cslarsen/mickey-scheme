/*
 * Mickey Scheme
 *
 * Copyright (C) 2015 Christian Stigen Larsen <csl@csl.name>
 * http://csl.name
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#ifndef INC_MICKEY_GC_H
#define INC_MICKEY_GC_H

#include "mickey.h"
#include "mickey/library.h"

size_t gc_status();
size_t gc_collect();
size_t gc_collect(cons_t* root);
size_t gc_collect(program_t* program);

cons_t* gc_alloc_cons(const type_t& type = NIL);

const symbol_t* gc_alloc_symbol(const char* name);
pointer_t* gc_alloc_pointer(const char* tag, void* ptr);
program_t* gc_alloc_program();
closure_t* gc_alloc_closure();

syntax_t* gc_alloc_syntax(cons_t* transformer = NULL,
                          environment_t* env = NULL);

environment_t* gc_alloc_environment();

vector_t* gc_alloc_vector();
vector_t* gc_alloc_vector(const vector_t* v);
vector_t* gc_alloc_vector(const size_t& size);
vector_t* gc_alloc_vector(const size_t& size, cons_t* fill);

bytevector_t* gc_alloc_bytevector();
bytevector_t* gc_alloc_bytevector(const bytevector_t* v);
bytevector_t* gc_alloc_bytevector(const size_t& size);
bytevector_t* gc_alloc_bytevector(const size_t& size, const uint8_t fill);
bytevector_t* gc_alloc_bytevector(const std::vector<uint8_t>& v);

char* gc_alloc_string(const char* source);
char* gc_alloc_string(const size_t length);

struct library_t;
library_t* gc_alloc_library();

void gc_add_root(const cons_t*);
void gc_add_root(environment_t*);
void gc_add_root(program_t*);

cons_t* make_bytevector(const size_t size = 0);
cons_t* make_bytevector(const size_t size, const uint8_t fill);
cons_t* make_symbol(const char* name);
cons_t* make_vector(const size_t size = 0, cons_t* fill = NULL);
cons_t* make_vector(const vector_t* v);
cons_t* make_pair(cons_t* car = NULL, cons_t* cdr = NULL);
cons_t* make_string(const char* s);

#endif
