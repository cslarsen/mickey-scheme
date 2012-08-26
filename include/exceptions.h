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

#ifndef INC_MICKEY_EXCEPTIONS_H
#define INC_MICKEY_EXCEPTIONS_H

#include <string>

struct exception_t
{
  exception_t()
  {
  }

  virtual ~exception_t()
  {
  }

  virtual const char* what() const = 0;
};

class general_exception : public exception_t
{
  std::string _what;
public:
  general_exception(const std::string& message = "Unknown exception")
    : _what(message)
  {
  }

  virtual ~general_exception() throw()
  {
  }

  const char* what() const throw() {
    return _what.c_str();
  }
};

/*
 * Use TRY { ... } and CATCH ( spec ) { ... } for
 * exception
 * handling.  
 *
 * This makes it possible to revert to longjumps
 * if C++ exception handling has been disabled.
 *
 * By the way, it's a dirty trick to use `extern´ to ignore
 * the catch specifier.
 *
 */
#define TRY                     \
    bool got_exception = false; \
    if ( exception_raised() ) { \
      got_exception = true;     \
      goto CATCH_POINT;        }

#define CATCH(args)                             \
  CATCH_POINT:                                  \
  args = general_exception(__exception.what());\
  if (got_exception)

/*
 * Raise error.
 */
void raise(const exception_t&);

#include <setjmp.h>
extern jmp_buf catch_point;
extern general_exception __exception;
#define exception_raised() setjmp(catch_point)

/*
 * Specific exceptions
 */

struct parser_exception : general_exception {
  parser_exception(const std::string& s) : general_exception("<parser> " + s) { }
};

struct compiler_exception : general_exception {
  compiler_exception(const std::string& s) : general_exception("<compiler> " + s) { }
};

struct syntax_error : general_exception {
  syntax_error(const std::string& s) : general_exception("<syntax> " + s) { }
};

struct unsupported_error : general_exception {
  unsupported_error(const std::string& s) : general_exception("<syntax> " + s) { }
};

struct runtime_exception : general_exception {
  runtime_exception(const std::string& s) : general_exception("<runtime> " + s) { }
};

#endif
