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

#include "mickey.h"

extern named_function_t exports_dlopen[];

cons_t* proc_dlclose(cons_t*, environment_t*);
cons_t* proc_dlerror(cons_t*, environment_t*);
cons_t* proc_dlopen(cons_t*, environment_t*);
cons_t* proc_dlopen_internal(cons_t*, environment_t*);
cons_t* proc_dlsym(cons_t*, environment_t*);
cons_t* proc_dlsym_syntax(cons_t*, environment_t*);
