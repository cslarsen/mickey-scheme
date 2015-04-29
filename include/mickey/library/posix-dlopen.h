/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2015 Christian Stigen Larsen
 * http://csl.name
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include "mickey/mickey.h"

extern "C" {

extern named_function_t exports_dlopen[];

cons_t* proc_dlclose(cons_t*, environment_t*);
cons_t* proc_dlerror(cons_t*, environment_t*);
cons_t* proc_dlopen(cons_t*, environment_t*);
cons_t* proc_dlopen_determine_extension(cons_t*, environment_t*);
cons_t* proc_dlopen_internal(cons_t*, environment_t*);
cons_t* proc_dlopen_internal_determine_extension(cons_t*, environment_t*);
cons_t* proc_dlopen_self(cons_t*, environment_t*);
cons_t* proc_dlsym(cons_t*, environment_t*);
cons_t* proc_dlsym_syntax(cons_t*, environment_t*);

}
