/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2012 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 * NOTE: The list below is by no means exhaustive.  If you find a way to
 * detect certain features, please be my guest in posting pull requests for
 * that.
 *
 */

#include <set>
#include <limits>
#include "system-features.h"
#include "mickey.h"

#ifdef __APPLE__
# include "TargetConditionals.h"
#endif

#ifdef __posix
# include <sys/utsname.h>
#endif

static std::set<std::string> features;

bool detect_operating_system();

extern "C" cons_t* proc_list_features(cons_t* p, environment_t*)
{
  assert_length(p, 0);
  cons_t* r = list(), *e = r;

  for ( std::set<std::string>::iterator i = features.begin();
        i != features.end(); ++i )
  {
    e->car = symbol((*i).c_str());
    e->cdr = cons(NULL);
    e = e->cdr;
  }

  return r;
}

bool supports_feature(const char* s)
{
  return features.count(s);
}

void add_feature(const char* s)
{
  features.insert(s);
}

void remove_feature(const char* s)
{
  features.erase(s);
}

/*
 * Comments taken from R7RS.
 *
 * Some compiler macros taken from:
 * - http://sourceforge.net/p/predef/wiki/OperatingSystems/
 * - http://nadeausoftware.com/articles/2012/01/c_c_tip_how_use_compiler_predefined_macros_detect_operating_system
 */
void detect_features()
{
  /*
   * All R7RS Scheme implementations have this feature.
   */
  add_feature("r7rs");

  /*
   * All algebraic operations except / produce exactvalues given exact
   * inputs.
   */
  //add_feature("exact-closed"); // TODO: Add unit tests for this!

  /*
   * / with exact arguments produces an exact result when the divisor is
   * nonzero.
   */
  // add_feature("ratios");  NOT SUPPORTED

  /*
   * Exact complex numbers are provided
   */
  // add_feature("exact-complex");

  /*
   * Inexact numbers are IEEE 754 floating point values.
   */
  if ( std::numeric_limits<real_t>::is_iec559 )
    add_feature("ieee-float");

  /*
   * All Unicode code points are supported as characters (except the
   * surogates)
   */
  // add_feature("full-unicode");

  bool os_found = detect_operating_system();

#ifdef __posix
  /*
   * Use uname to query system
   */
  struct utsname u;

  if ( !uname(&u) ) {

    /*
     * Add OS name if not already detected
     */
    if ( !os_found )
      add_feature(tolower(u.sysname));

    std::string machine = tolower(u.machine);
    if ( machine == "x86_64" ) add_feature("x86-64");
    // ... TODO add more here

    /*
     * TODO: Detect the following
     * i386, x86-64, ppc, sparc, jvm, clr, llvm, ...
     */
  }
#endif

  if ( !os_found )
    add_feature("unknown-operating-system");

  /*
   * TODO: Detect all of the following
   * unix, darwin, linux, bsd, freebsd, solaris, ...
   */

  /*
   * C memory model flags: ilp32, lp64, ilp64, ...
   *
   * TODO: Detect these.
   */

  /*
   * Byte order flags.  Detect endianness.
   */
  {
    uint32_t w = 0x12345678;
    uint16_t *h = reinterpret_cast<uint16_t*>(&w);

    if ( *h == 0x1234 ) // big end first
      add_feature("big-endian");
    else if ( *h == 0x5678 ) // little end first
      add_feature("little-endian");
  }

  /*
   * The name and version of this implementation
   */
  add_feature("mickey"); 
  add_feature("mickey-0.0");
}


/*
 * Detect operating system, return true if found.
 *
 * Some predefined macros can be found at:
 * http://sourceforge.net/apps/mediawiki/predef/index.php?title=Operating_Systems
 *
 * A good list is:
 * http://nadeausoftware.com/articles/2012/01/c_c_tip_how_use_compiler_predefined_macros_detect_operating_system
 */
bool detect_operating_system()
{
  bool os_found = false;

  /*
   * This Scheme implementation is running on a POSIX system.
   */
#ifdef __posix
  add_feature("posix");
#endif

  /*
   * This Scheme implementation is running on Windows.
   */
#ifdef __WIN32__
  add_feature("windows");
  add_feature("win32");
  os_found = true;
#endif

#ifdef __WIN64__
  add_feature("win64");
  os_found = true;
#endif

  /*
   * Operating system flags (more than one may apply).
   */
#if !defined(_WIN32) && (defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__)))
  add_feature("unix");
#endif

#ifdef __linux
  add_feature("linux");
  os_found = true;
#endif

#ifdef __APPLE__
  add_feature("darwin");
  os_found = true;
#endif

#ifdef BSD
  add_feature("bsd");
#endif

#ifdef __FreeBSD__
  add_feature("freebsd");
  os_found = true;
#endif

#ifdef __NetBSD__
  add_feature("netbsd");
  os_found = true;
#endif

#ifdef __OpenBSD__
  add_feature("openbsd");
  os_found = true;
#endif

#ifdef __CYGWIN__
  add_feature("cygwin");
#endif

  /*
   * Distinguish Solaris and SunOS, taken from
   * http://sourceforge.net/apps/mediawiki/predef/index.php?title=Operating_Systems#Solaris
   */
#if defined(sun) ||defined(__sun)
# if defined(__SVR4) || defined(__svr4__)
  add_feature("solaris");
  os_found = true;
#else
  add_feature("sunos");
  os_found = true;
# endif
#endif

#if defined(__sysv__) || defined(__SVR4) || defined(__svr4__) || defined(_SYSTYPE_SVR4)
  add_feature("svr4");
#endif

#if defined(_hpux) || defined(hpux) || defined(__hpux)
  add_feature("hp-ux");
  os_found = true;
#endif

#if defined(sgi) || defined(__sgi)
  add_feature("irix");
  os_found = true;
#endif

  return os_found;
}
