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
#include <limits>
#include <stdlib.h>
#include <inttypes.h>
#include "assertions.h"
#include "exceptions.h"
#include "primitives.h"
#include "print.h"

/*
 * Convert a number to given data type and check for numerical limits.
 */
template<typename T>
static T convert(const cons_t* n)
{
  if ( integerp(n) ) {
    integer_t i = n->number.integer;

    if ( i > std::numeric_limits<T>::max() ||
         i < std::numeric_limits<T>::min() )
    {
      raise(runtime_exception("Integer does not fit target type"));
    }

    return static_cast<T>(i);
  }

  if ( realp(n) ) {
    real_t f = n->number.real;

    if ( f > std::numeric_limits<T>::max() ||
         f < std::numeric_limits<T>::min() )
    {
      raise(runtime_exception("Real number does not fit target type"));
    }

    return static_cast<T>(f);
  }

  if ( rationalp(n) ) {
    real_t f = static_cast<real_t>(n->number.rational.numerator) /
               static_cast<real_t>(n->number.rational.denominator);

    if ( f > std::numeric_limits<T>::max() ||
         f < std::numeric_limits<T>::min() )
    {
      raise(runtime_exception("Rational number does not fit target type"));
    }

    // TODO: We shouldn't just truncate the number here
    return static_cast<T>(n->number.rational.numerator /
                          n->number.rational.denominator);
  }

  raise(runtime_exception(format("Cannot convert to integer type: %s",
          sprint(n).c_str())));
  return 0;
}

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
 * (make-<tag>vector size [fill])
 */
#define MAKE_VECTOR(SHORT_TAG, PTR_TAG, DATA_TYPE) \
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
  DATA_TYPE fill = convert<DATA_TYPE>(f); \
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
#define VECTOR(SHORT_TAG, PTR_TAG, DATA_TYPE) \
extern "C" cons_t* SHORT_TAG ## vector(cons_t* p, environment_t*) \
{ \
  const size_t size = length(p); \
  size_t pos = 0; \
  \
  std::vector<DATA_TYPE> *v = new std::vector<DATA_TYPE>(size); \
  \
  while ( !nullp(p) ) { \
    v->operator[](pos++) = convert<DATA_TYPE>(car(p)); \
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
#define VECTOR_SET(SHORT_TAG, PTR_TAG, DATA_TYPE) \
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
  \
  if ( k<0 || static_cast<size_t>(k) >= v->size() ) \
    raise(runtime_exception(format("%s index out of range", PTR_TAG))); \
  \
  v->operator[](k) = convert<DATA_TYPE>(caddr(p)); /* note: val is integer */ \
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

MAKE_VECTOR(s8,  TAG_S8,    int8_t);
MAKE_VECTOR(u8,  TAG_U8,   uint8_t);
MAKE_VECTOR(s16, TAG_S16,  int16_t);
MAKE_VECTOR(u16, TAG_U16, uint16_t);
MAKE_VECTOR(s32, TAG_S32,  int32_t);
MAKE_VECTOR(u32, TAG_U32, uint32_t);
MAKE_VECTOR(s64, TAG_S64,  int64_t);
MAKE_VECTOR(u64, TAG_U64, uint64_t);

VECTORP(s8,  TAG_S8,   int8_t);
VECTORP(u8,  TAG_U8,  uint8_t);
VECTORP(s16, TAG_S16,  int16_t);
VECTORP(u16, TAG_U16, uint16_t);
VECTORP(s32, TAG_S32,  int32_t);
VECTORP(u32, TAG_U32, uint32_t);
VECTORP(s64, TAG_S64,  int64_t);
VECTORP(u64, TAG_U64, uint64_t);

VECTOR(s8,  TAG_S8,    int8_t);
VECTOR(u8,  TAG_U8,   uint8_t);
VECTOR(s16, TAG_S16,  int16_t);
VECTOR(u16, TAG_U16, uint16_t);
VECTOR(s32, TAG_S32,  int32_t);
VECTOR(u32, TAG_U32, uint32_t);
VECTOR(s64, TAG_S64,  int64_t);
VECTOR(u64, TAG_U64, uint64_t);

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

VECTOR_SET(s8,  TAG_S8,    int8_t);
VECTOR_SET(u8,  TAG_U8,   uint8_t);
VECTOR_SET(s16, TAG_S16,  int16_t);
VECTOR_SET(u16, TAG_U16, uint16_t);
VECTOR_SET(s32, TAG_S32,  int32_t);
VECTOR_SET(u32, TAG_U32, uint32_t);
VECTOR_SET(s64, TAG_S64,  int64_t);
VECTOR_SET(u64, TAG_U64, uint64_t);

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
