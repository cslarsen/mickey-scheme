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

#include <stdio.h>
#include <string.h>

/*
 * Port types
 */
enum porttype_t {
  TEXTUAL_PORT,
  BINARY_PORT
};

/*
 * An input or output port to either files or strings.
 *
 * TODO: Remove constructor methods, add a pointer to current read/write
 * position in string to enable reading/writing strings.
 *
 * Check necessary options when performing a read/write.
 */
class port_t
{
  FILE *f;
  char *s;
public:
  porttype_t port_type;
  bool writable, readable;

  port_t() :
    f(NULL),
    s(NULL),
    port_type(TEXTUAL_PORT),
    writable(false),
    readable(false)
  {
  }

  port_t(FILE* file) :
    f(file),
    s(NULL),
    port_type(TEXTUAL_PORT),
    writable(false),
    readable(false)
  {
  }

  port_t(const port_t& r)
  {
    if ( this != &r )
      memcpy(this, &r, sizeof(port_t));
  }

  port_t& textual()
  {
    port_type = TEXTUAL_PORT;
    return *this;
  }

  port_t& input()
  {
    readable = true;
    return *this;
  }

  port_t& output()
  {
    writable = true;
    return *this;
  }

  bool fileport() const
  {
    return f!=NULL && s==NULL;
  }

  bool stringport() const
  {
    return f==NULL && s!=NULL;
  }

  FILE* file() const
  {
    return f;
  }

  bool isopen() const
  {
    // file port
    if ( f != NULL )
      return ftell(f) != -1; // TODO: Is there a better way to check?

    // string port
    if ( s != NULL )
      return writable || readable;

    return false;
  }

  bool iswritable() const
  {
    return writable;
  }

  bool isreadable() const
  {
    return readable;
  }

  bool istextual() const
  {
    return port_type == TEXTUAL_PORT;
  }

  bool isbinary() const
  {
    return port_type == BINARY_PORT;
  }

  friend bool operator==(const port_t& l, const port_t& r)
  {
    return l.f == r.f && l.port_type == r.port_type &&
      l.writable == r.writable && l.readable == r.readable &&
      l.s == r.s;
  }

  port_t& operator=(const port_t& r)
  {
    if ( this != &r )
      memcpy(this, &r, sizeof(port_t));
    return *this;
  }

  void close()
  {
    if ( f != NULL ) {
      fclose(f);
      readable = writable = false;
    }

    if ( s != NULL ) {
      s = NULL;
      readable = writable = false;
    }
  }
};
