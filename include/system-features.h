/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-201 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#ifndef INC_MICKEY_FEATURES_H
#define INC_MICKEY_FEATURES_H

#include "cons.h"

extern "C" cons_t* proc_list_features(cons_t*, environment_t*);

bool supports_feature(const char*);
void add_feature(const char*);
void detect_features();
void remove_feature(const char*);

#endif
