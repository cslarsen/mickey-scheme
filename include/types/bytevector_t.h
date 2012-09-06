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

#include <stdint.h>
#include <vector>

/*
 * Array of unsigned 8-bit integer values.
 */
struct bytevector_t {
  std::vector<uint8_t> bytevector;

  bytevector_t()
  {
  }

  bytevector_t(const bytevector_t& v) : bytevector(v.bytevector)
  {
  }

  bytevector_t(size_t size) : bytevector(size)
  {
  }

  bytevector_t(size_t size, const uint8_t fill) : bytevector(size, fill)
  {
  }

  bytevector_t(const std::vector<uint8_t>& v) : bytevector(v)
  {
  }
};
