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

#include "util.h"
#include "parser.h"
#include "print.h"
#include "exceptions.h"
#include "platform-limits.h"

/*
 * Get next character in string, if any left, and advance pointer.
 * Returns '\0' if no chars available.
 */
static char getnext(const char** src, const char* end)
{
  char r = '\0';

  if ( *src < end ) {
    r = **src;
    ++*src;
  }

  return r;
}

/*
 * Convert hexadecimal character to a number.
 */
static uint8_t hexval(int c)
{
  if ( c>='0' && c<='9' )
    return c - '0';

  c = tolower(c);
  if ( c>='a' && c<='f' )
    return 10 + c - 'a';

  if ( isprint(c) )
    raise(parser_exception(format("Not a hexadecimal digit: '%c'",
      static_cast<char>(c))));
  else
    raise(parser_exception(format("Not a hexadecimal digit: \\x%x", c)));

  return 0; // to please compiler
}

static std::string decode_literal_string(const char* s)
{
  std::string r;

  const size_t skip_quote = 1;
  const char *end = s+strlen(s) - skip_quote;

  for ( const char *p = s+skip_quote; p < end; ++p ) {
    // add normal characters to output
    if ( *p != '\\' ) {
      r += *p;
      continue;
    }

    // advance to next char
    if ( !(p++ < end) ) {
      // note: technically, this is not supported because of tests
      // done on the string before decode_literal_string
      raise(parser_exception("String did not end with double-quote"));
      break;
    }

    switch ( *p ) {
      // known escape characters
      case 'a': r += '\a'; break;
      case 'b': r += '\b'; break;
      case 't': r += '\t'; break;
      case 'n': r += '\n'; break;
      case 'r': r += '\r'; break;
      case '"': r += '"'; break;
      case '\\': r += '\\'; break;
      case '|': r += '|'; break;

      case 'x': { // format "\x<hex>;"
        const char * start = p;
        // skip 'x'
        if ( !(p++ < end) )
          raise(parser_exception("Incomplete \\x<hex>; sequence in string"));

        // decode "\x<hex>;" formatted character
        uint16_t value = 0, pos = 0;
        char c = '\0';

        // atoi, the right way
        while ( (c = getnext(&p, end)) != '\0' && c != ';' ) {
          ++pos;

          uint16_t prev = value;
          value = value*16 + hexval(c);

          // detect overflow
          if ( value < prev ) {
            while ( *p!=0 && *p!=';' )
              ++pos, ++p;

            raise(parser_exception(format(
              "Unicode character does not fit into unsigned %u-bit storage: \\%.*s",
                sizeof(value)*8, pos+2, start))); // +2 == the two chars \x
          }
        }

        if ( c != ';' ) {
          raise(parser_exception(format(
            "Character escape sequence not semi-colon terminated: \\%.*s",
              pos+1, start)));
        }

        if ( value > MAX_UNICODE_CHAR ) {
          raise(parser_exception(format(
            "Unicode character not supported on this platform: U+%.*X",
              sizeof(value)*8/4, value)));
        }

        r += static_cast<char>(value);
        --p; // back up one position for the loop increment
      } break;

      default:
        // unknown escape character; just add a default value (ref. R7RS
        // chapter 6.7)
        r += *p;
        break;
    }
  }

  return r;
}

cons_t* parse_string(const char* s)
{
  return string(decode_literal_string(s).c_str());
}
