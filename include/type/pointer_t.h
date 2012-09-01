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
 * A tagged void pointer.  Used for hairy stuff like dynamic loading (hairy
 * because we don't normally want to expose this to the scheme environment,
 * and because converting function pointers to and from void pointers is not
 * guaranteed by the C standard to work).
 */
struct pointer_t {
  const char* tag;
  void* value;

  pointer_t(const char* tag_, void* ptr)
    : tag(tag_), value(ptr)
  {
  }

  friend bool operator==(const pointer_t& l, const pointer_t& r)
  {
    return l.tag == r.tag && l.value == r.value;
  }
};
