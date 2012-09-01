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

/*
 * Function signatures for all C procedures; they all take a cons_t* and an
 * environment and always returns cons_t*.
 */
typedef struct cons_t* (*lambda_t)(struct cons_t*, struct environment_t*);
