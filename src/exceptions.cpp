/*
 * Mickey Scheme
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *                                                          
 */

#include "exceptions.h"

jmp_buf catch_point;
general_exception __exception;

void raise(const exception_t& e)
{
  __exception = general_exception(e.what());
  longjmp(catch_point, 1);
}
