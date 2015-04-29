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

#include <string>

/*
 * A symbol is nothing else but a string.
 */
typedef std::string symbol_t;

/*
 * Symbols are uniquely identified by their names, so all symbols with equal
 * names will actually point to the same memory location.
 */
const symbol_t* create_symbol(const std::string& s);
