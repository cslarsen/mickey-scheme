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
 * Basic and extended Scheme types.
 */
enum type_t {
  NIL,
  BOOLEAN,
  CHAR,
  INTEGER,
  RATIONAL,
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
