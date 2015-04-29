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

#ifndef INC_MICKEY_TOKENIZER_H
#define INC_MICKEY_TOKENIZER_H

bool get_fold_case();
const char* get_token();
int current_line_number();
void set_fold_case(bool setting);
void set_source(const char* program);

#endif
