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

extern "C" {

cons_t* proc_char_alphabeticp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(CHAR, car(p));
  return boolean(isalpha(car(p)->character));
}

cons_t* proc_char_numericp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(CHAR, car(p));
  return boolean(isdigit(car(p)->character));
}

cons_t* proc_char_lowercasep(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(CHAR, car(p));
  return boolean(islower(car(p)->character));
}

cons_t* proc_char_uppercasep(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(CHAR, car(p));
  return boolean(isupper(car(p)->character));
}

cons_t* proc_char_upcase(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(CHAR, car(p));
  return character(toupper(car(p)->character));
}

cons_t* proc_char_downcase(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(CHAR, car(p));
  return character(tolower(car(p)->character));
}

cons_t* proc_char_whitespacep(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(CHAR, car(p));
  return boolean(isspace(car(p)->character));
}

/*
 * exports
 */
named_function_t exports_char[] = {
  {"char-alphabetic?", proc_char_alphabeticp, false},
  {"char-downcase", proc_char_downcase, false},
  {"char-lower-case?", proc_char_lowercasep, false},
  {"char-numeric?", proc_char_numericp, false},
  {"char-upcase", proc_char_upcase, false},
  {"char-upper-case?", proc_char_uppercasep, false},
  {"char-whitespace?", proc_char_whitespacep, false},
  {NULL, NULL, false}
};

};
