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

#include <vector>

/*
 * Arrays.
 */
struct vector_t {
  std::vector<cons_t*> vector;

  vector_t()
  {
  }

  vector_t(const vector_t& v)
  {
    if ( this != &v )
      vector = v.vector;
  }

  vector_t(size_t size) : vector(size)
  {
  }

  vector_t(size_t size, cons_t* fill) : vector(size, fill)
  {
  }
};
