/*
 * Mickey Scheme
 *
 * Copyright (C) 2012-2013 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include <iostream>
#include <vector>
#include <stdlib.h>
#include <inttypes.h>
#include "assertions.h"
#include "exceptions.h"
#include "primitives.h"
#include "print.h"

/*
 * A SIGNED data type large enough to hold both signed and usigned 64-bit
 * numbers.
 *
 * If you do not have __int128_t on your system, use the closest SIGNED type
 * you can find, or submit a patch.
 */
typedef __int128_t large_t;

/*
 * Pointer tags (used internally to discern between void pointers).
 */
#define TAG_S8 "s8vector"
#define TAG_U8 "u8vector"
#define TAG_S16 "s16vector"
#define TAG_U16 "u16vector"
#define TAG_S32 "s32vector"
#define TAG_U32 "u32vector"
#define TAG_S64 "s64vector"
#define TAG_U64 "u64vector"

/*
 * Minimum and maximum values given data types can hold (assuming two's
 * complement).
 */
#define MIN_S8 -128
#define MAX_S8  127
#define MIN_U8  0
#define MAX_U8  255
 //
#define MIN_S16 -32768
#define MAX_S16  32767
#define MIN_U16 0
#define MAX_U16 65536
 //
#define MIN_S32 -2147383647 /* -2^31 + 1 */
#define MAX_S32  2147483647 /*  2^31 - 1 */
#define MIN_U32 0
#define MAX_U32 4294967295  /*  2^32 - 1 */
 //
#define MIN_S64 -9223372036854775807 /* -2^63 + 1 */
#define MAX_S64  9223372036854775807 /*  2^63 - 1 */
#define MIN_U64 0
#define MAX_U64 18446744073709551615UL /* 2^64 - 1 */

/*
 * (make-<tag>vector size [fill])
 */
#define MAKE_VECTOR(SHORT_TAG, PTR_TAG, DATA_TYPE, MIN_VAL, MAX_VAL) \
extern "C" cons_t* make_ ## SHORT_TAG ## \
                   vector(cons_t* p, environment_t*) \
{ \
  assert_length(p, 1, 2); \
  \
  assert_type(INTEGER, car(p)); \
  const size_t size = car(p)->number.integer; \
  \
  if ( length(p) < 2 ) \
    return pointer(PTR_TAG, new std::vector<DATA_TYPE>(size)); \
  \
  cons_t *f = cadr(p); \
  assert_number(f); \
  \
  large_t n = 0; \
  if ( integerp(f) )  n = f->number.integer; else \
  if ( realp(f) )     n = f->number.real; else \
  if ( rationalp(f) ) n = (DATA_TYPE) f->number.rational.numerator / \
                          (DATA_TYPE) f->number.rational.denominator; \
  else { \
    raise(runtime_exception(format("Cannot fill %s with %s", \
      PTR_TAG, to_s(type_of(f)).c_str()))); \
  } \
  \
  if ( n<MIN_VAL || n>MAX_VAL ) \
    raise(runtime_exception(format("Value %s is too large to fit in %s", \
      sprint(f).c_str(), PTR_TAG))); \
  \
  DATA_TYPE fill = static_cast<DATA_TYPE>(n); \
  return pointer(PTR_TAG, new std::vector<DATA_TYPE>(size, fill)); \
}

/*
 * (<tag>vector? obj)
 */
#define VECTORP(SHORT_TAG, PTR_TAG, DATA_TYPE) \
extern "C" cons_t* SHORT_TAG ## vectorp(cons_t* p, environment_t*) \
{ \
  assert_length(p, 1); \
  return boolean(pointerp(PTR_TAG, car(p))); \
}

/*
 * (<tag>vector ...)
 */
#define VECTOR(SHORT_TAG, PTR_TAG, DATA_TYPE, MIN_VAL, MAX_VAL) \
extern "C" cons_t* SHORT_TAG ## vector(cons_t* p, environment_t*) \
{ \
  const size_t size = length(p); \
  size_t pos = 0; \
  \
  std::vector<DATA_TYPE> *v = new std::vector<DATA_TYPE>(size); \
  \
  while ( !nullp(p) ) { \
    cons_t *f = car(p); \
    large_t n = 0; \
    \
    if ( integerp(f) )  n = f->number.integer; else \
    if ( realp(f) )     n = f->number.real; else \
    if ( rationalp(f) ) n = (large_t) f->number.rational.numerator / \
                            (large_t) f->number.rational.denominator; \
    else { \
      raise(runtime_exception(format("Cannot put %s into %s", \
        to_s(type_of(f)).c_str(), PTR_TAG))); \
    } \
    \
    if ( n<MIN_VAL || n>MAX_VAL ) \
      raise(runtime_exception(format("Value %s is too large to fit in %s", \
        sprint(f).c_str(), PTR_TAG))); \
    \
    v->operator[](pos++) = static_cast<DATA_TYPE>(n); \
    p = cdr(p); \
  } \
  \
  return pointer(PTR_TAG, v); \
}

/*
 * (<tag>vector-length obj)
 */
#define VECTOR_LENGTH(SHORT_TAG, PTR_TAG, DATA_TYPE) \
extern "C" cons_t* SHORT_TAG ## vector_length(cons_t* p, environment_t*) \
{ \
  assert_length(p, 1); \
  assert_pointer(PTR_TAG, car(p)); \
  \
  std::vector<DATA_TYPE>* v = \
    static_cast<std::vector<DATA_TYPE>*>(car(p)->pointer->value); \
  \
  return integer(v->size()); \
}

/*
 * (<tag>vector-ref obj index)
 */
#define VECTOR_REF(SHORT_TAG, PTR_TAG, DATA_TYPE) \
extern "C" cons_t* SHORT_TAG ## vector_ref(cons_t* p, environment_t*) \
{ \
  assert_length(p, 2); \
  assert_pointer(PTR_TAG, car(p)); \
  assert_type(INTEGER, cadr(p)); \
  \
  std::vector<DATA_TYPE>* v = \
    static_cast<std::vector<DATA_TYPE>*>(car(p)->pointer->value); \
  \
  int k = cadr(p)->number.integer; \
  \
  if ( k<0 || static_cast<size_t>(k) >= v->size() ) \
    raise(runtime_exception(format("%s index out of range", PTR_TAG))); \
  \
  return integer(v->at(k)); \
}

/*
 * (<tag>vector-set! obj index value)
 */
#define VECTOR_SET(SHORT_TAG, PTR_TAG, DATA_TYPE, MIN_VAL, MAX_VAL) \
extern "C" cons_t* SHORT_TAG ## vector_set(cons_t* p, environment_t*) \
{ \
  assert_length(p, 3); \
  assert_pointer(PTR_TAG, car(p)); \
  assert_type(INTEGER, cadr(p)); \
  assert_type(INTEGER, caddr(p)); \
 \
  std::vector<DATA_TYPE>* v = \
    static_cast<std::vector<DATA_TYPE>*>(car(p)->pointer->value); \
  \
  int k = cadr(p)->number.integer; \
  int val = caddr(p)->number.integer; \
  \
  if ( k<0 || static_cast<size_t>(k) >= v->size() ) \
    raise(runtime_exception(format("%s index out of range", PTR_TAG))); \
  \
  large_t n = 0; \
  if ( n<MIN_VAL || n>MAX_VAL ) \
    raise(runtime_exception(format("Value %s is too large to fit in %s", \
      sprint(caddr(p)).c_str(), PTR_TAG))); \
  \
  v->operator[](k) = static_cast<DATA_TYPE>(val); \
  return unspecified(); \
}

/*
 * (list-><tag>vector proper-list)
 */
#define LIST_TO_VECTOR(SHORT_TAG, PTR_TAG, DATA_TYPE) \
extern "C" cons_t* list_to_ ## SHORT_TAG ## vector( \
                     cons_t* p, environment_t* e) \
{ \
  assert_length(p, 1); \
  assert_proper_list(car(p)); \
  return SHORT_TAG ## vector(car(p), e); \
}

/*
 * (<tag>vector->list <tag>vector)
 */
#define VECTOR_TO_LIST(SHORT_TAG, PTR_TAG, DATA_TYPE) \
extern "C" cons_t* SHORT_TAG ## vector_to_list( \
                     cons_t* p, environment_t*) \
{ \
  assert_length(p, 1); \
  assert_pointer(PTR_TAG, car(p)); \
  \
  std::vector<DATA_TYPE>* v = \
    static_cast<std::vector<DATA_TYPE>*>(car(p)->pointer->value); \
  \
  cons_t* r = list(); \
  \
  for ( std::vector<DATA_TYPE>::const_reverse_iterator i = \
        v->rbegin(); i != v->rend(); ++i ) \
  { \
    r = cons(integer(*i, true), r); \
  } \
  \
  return r; \
}

MAKE_VECTOR(s8,  TAG_S8,    int8_t, MIN_S8,  MAX_S8);
MAKE_VECTOR(u8,  TAG_U8,   uint8_t, MIN_U8,  MAX_U8);
MAKE_VECTOR(s16, TAG_S16,  int16_t, MIN_S16, MAX_S16);
MAKE_VECTOR(u16, TAG_U16, uint16_t, MIN_U16, MAX_U16);
MAKE_VECTOR(s32, TAG_S32,  int32_t, MIN_S32, MAX_S32);
MAKE_VECTOR(u32, TAG_U32, uint32_t, MIN_U32, MAX_U32);
MAKE_VECTOR(s64, TAG_S64,  int64_t, MIN_S64, MAX_S64);
MAKE_VECTOR(u64, TAG_U64, uint64_t, MIN_U64, MAX_U64);

VECTORP(s8,  TAG_S8,   int8_t);
VECTORP(u8,  TAG_U8,  uint8_t);
VECTORP(s16, TAG_S16,  int16_t);
VECTORP(u16, TAG_U16, uint16_t);
VECTORP(s32, TAG_S32,  int32_t);
VECTORP(u32, TAG_U32, uint32_t);
VECTORP(s64, TAG_S64,  int64_t);
VECTORP(u64, TAG_U64, uint64_t);

VECTOR(s8,  TAG_S8,    int8_t, MIN_S8,  MAX_S8);
VECTOR(u8,  TAG_U8,   uint8_t, MIN_U8,  MAX_U8);
VECTOR(s16, TAG_S16,  int16_t, MIN_S16, MAX_S16);
VECTOR(u16, TAG_U16, uint16_t, MIN_U16, MAX_U16);
VECTOR(s32, TAG_S32,  int32_t, MIN_S32, MAX_S32);
VECTOR(u32, TAG_U32, uint32_t, MIN_U32, MAX_U32);
VECTOR(s64, TAG_S64,  int64_t, MIN_S64, MAX_S64);
VECTOR(u64, TAG_U64, uint64_t, MIN_U64, MAX_U64);

VECTOR_LENGTH(s8,  TAG_S8,   int8_t);
VECTOR_LENGTH(u8,  TAG_U8,  uint8_t);
VECTOR_LENGTH(s16, TAG_S16,  int16_t);
VECTOR_LENGTH(u16, TAG_U16, uint16_t);
VECTOR_LENGTH(s32, TAG_S32,  int32_t);
VECTOR_LENGTH(u32, TAG_U32, uint32_t);
VECTOR_LENGTH(s64, TAG_S64,  int64_t);
VECTOR_LENGTH(u64, TAG_U64, uint64_t);

VECTOR_REF(s8,  TAG_S8,   int8_t);
VECTOR_REF(u8,  TAG_U8,  uint8_t);
VECTOR_REF(s16, TAG_S16,  int16_t);
VECTOR_REF(u16, TAG_U16, uint16_t);
VECTOR_REF(s32, TAG_S32,  int32_t);
VECTOR_REF(u32, TAG_U32, uint32_t);
VECTOR_REF(s64, TAG_S64,  int64_t);
VECTOR_REF(u64, TAG_U64, uint64_t);

VECTOR_SET(s8,  TAG_S8,    int8_t, MIN_S8,  MAX_S8);
VECTOR_SET(u8,  TAG_U8,   uint8_t, MIN_U8,  MAX_U8);
VECTOR_SET(s16, TAG_S16,  int16_t, MIN_S16, MAX_S16);
VECTOR_SET(u16, TAG_U16, uint16_t, MIN_U16, MAX_U16);
VECTOR_SET(s32, TAG_S32,  int32_t, MIN_S32, MAX_S32);
VECTOR_SET(u32, TAG_U32, uint32_t, MIN_U32, MAX_U32);
VECTOR_SET(s64, TAG_S64,  int64_t, MIN_S64, MAX_S64);
VECTOR_SET(u64, TAG_U64, uint64_t, MIN_U64, MAX_U64);

LIST_TO_VECTOR(s8,  TAG_S8,   int8_t);
LIST_TO_VECTOR(u8,  TAG_U8,  uint8_t);
LIST_TO_VECTOR(s16, TAG_S16,  int16_t);
LIST_TO_VECTOR(u16, TAG_U16, uint16_t);
LIST_TO_VECTOR(s32, TAG_S32,  int32_t);
LIST_TO_VECTOR(u32, TAG_U32, uint32_t);
LIST_TO_VECTOR(s64, TAG_S64,  int64_t);
LIST_TO_VECTOR(u64, TAG_U64, uint64_t);

VECTOR_TO_LIST(s8,  TAG_S8,   int8_t);
VECTOR_TO_LIST(u8,  TAG_U8,  uint8_t);
VECTOR_TO_LIST(s16, TAG_S16,  int16_t);
VECTOR_TO_LIST(u16, TAG_U16, uint16_t);
VECTOR_TO_LIST(s32, TAG_S32,  int32_t);
VECTOR_TO_LIST(u32, TAG_U32, uint32_t);
VECTOR_TO_LIST(s64, TAG_S64,  int64_t);
VECTOR_TO_LIST(u64, TAG_U64, uint64_t);
