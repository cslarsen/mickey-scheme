/*
 * Mickey Scheme
 *
 * Copyright (C) 2012 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include <vector>
#include <stdlib.h>
#include <inttypes.h>
#include "assertions.h"
#include "primitives.h"

#define TAG_S8 "s8vector"
#define TAG_U8 "u8vector"
#define TAG_S16 "s16vector"
#define TAG_U16 "u16vector"
#define TAG_S32 "s32vector"
#define TAG_U32 "u32vector"
#define TAG_S64 "s64vector"
#define TAG_U64 "u64vector"

#define MAKE_VECTOR(SHORT_TAG, PTR_TAG, DATA_TYPE) \
  extern "C" cons_t* make_ ## SHORT_TAG ## vector(cons_t*, environment_t*) \
  { \
    return pointer(PTR_TAG, new std::vector<DATA_TYPE>()); \
  }

#define VECTORP(SHORT_TAG, PTR_TAG, DATA_TYPE) \
  extern "C" cons_t* SHORT_TAG ## vectorp(cons_t* p, environment_t*) \
  { \
    assert_length(p, 1); \
    return boolean(pointerp(PTR_TAG, car(p))); \
  }

MAKE_VECTOR(s8, TAG_S8, int8_t);
MAKE_VECTOR(u8, TAG_U8, uint8_t);
MAKE_VECTOR(s16, TAG_S16, int16_t);
MAKE_VECTOR(u16, TAG_U16, uint16_t);
MAKE_VECTOR(s32, TAG_S32, int32_t);
MAKE_VECTOR(u32, TAG_U32, uint32_t);
MAKE_VECTOR(s64, TAG_S64, int64_t);
MAKE_VECTOR(u64, TAG_U64, uint64_t);

VECTORP(s8, TAG_S8, int8_t);
VECTORP(u8, TAG_U8, uint8_t);
VECTORP(s16, TAG_S16, int16_t);
VECTORP(u16, TAG_U16, uint16_t);
VECTORP(s32, TAG_S32, int32_t);
VECTORP(u32, TAG_U32, uint32_t);
VECTORP(s64, TAG_S64, int64_t);
VECTORP(u64, TAG_U64, uint64_t);
