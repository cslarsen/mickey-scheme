/*
 * UNIX wait for Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under the LGPL 2.1; see LICENSE
 */

#include <time.h>
#include <errno.h>
#include <string.h>
#include "mickey/mickey.h"

static const char* id_time_t = "<time.h> time_t";
static const char* id_struct_tm = "<time.h> struct tm";

extern "C" cons_t* proc_time(cons_t* p, environment_t*)
{
  assert_length(p, 0, 1);

  time_t *t = new time_t(0);

  if ( length(p) == 1 ) {
    assert_type(INTEGER, car(p));
    *t = car(p)->number.integer;
  } else {
    if ( time(t) == static_cast<time_t>(-1) )
      raise(runtime_exception(format(
        "time(): %s", strerror(errno))));
  }

  return pointer(id_time_t, t);
}

extern "C" cons_t* proc_time_value(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer(id_time_t, car(p));
  time_t *t = static_cast<time_t*>(car(p)->pointer->value);
  return integer(static_cast<long>(*t));
}

extern "C" cons_t* proc_ctime(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer(id_time_t, car(p));
  time_t *t = static_cast<time_t*>(car(p)->pointer->value);

  char buf[32];
  memset(buf, '\0', sizeof(buf));

  ctime_r(t, buf);
  return string(buf);
}

extern "C" cons_t* proc_localtime(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer(id_time_t, car(p));
  time_t *t = static_cast<time_t*>(car(p)->pointer->value);

  struct tm* local = new struct tm();
  localtime_r(t, local);

  return pointer(id_struct_tm, local);
}

extern "C" cons_t* proc_gmtime(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer(id_time_t, car(p));
  time_t *t = static_cast<time_t*>(car(p)->pointer->value);

  struct tm* local = new struct tm();
  gmtime_r(t, local);

  return pointer(id_struct_tm, local);
}

static cons_t* day_symbols(const int day)
{
  static const char* days[] = {
    "sunday",
    "monday",
    "tuesday",
    "wednesday",
    "thursday",
    "friday",
    "saturday"
  };

  if ( day<0 || day>6 )
    raise(runtime_exception(format(
      "day_symbols() argument out of bounds: %d", day)));

  return symbol(days[day]);
}

static cons_t* month_symbol(const int month)
{
  static const char* months[] = {
    "january",
    "february",
    "march",
    "april",
    "may",
    "june",
    "july",
    "august",
    "september",
    "october",
    "november",
    "december"
  };

  if ( month<0 || month>11 )
    raise(runtime_exception(format(
      "month_symbol() argument out of bounds: %d", month)));

  return symbol(months[month]);
}

extern "C" cons_t* proc_tm_to_alist(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_pointer(id_struct_tm, car(p));
  struct tm* t = static_cast<struct tm*>(car(p)->pointer->value);

  cons_t *r =
    cons(list(symbol("seconds"), integer(t->tm_sec)),
    cons(list(symbol("minutes"), integer(t->tm_min)),
    cons(list(symbol("hours"), integer(t->tm_hour)),
    cons(list(symbol("month-day"), integer(t->tm_mday)),
    cons(list(symbol("month"), month_symbol(t->tm_mon)),
    cons(list(symbol("year"), integer(1900 + t->tm_year)),
    cons(list(symbol("week-day"), day_symbols(t->tm_wday)),
    cons(list(symbol("year-day"), integer(t->tm_yday)),
    cons(list(symbol("dst?"), boolean(t->tm_isdst > 0)),
    cons(list(symbol("timezone"), symbol(t->tm_zone)),
    cons(list(symbol("utc-offset-seconds"), integer(t->tm_gmtoff)))))))))))));

  return r;
}

//  int tm_sec;     /* seconds (0 - 60) */
//  int tm_min;     /* minutes (0 - 59) */
//  int tm_hour;    /* hours (0 - 23) */
//  int tm_mday;    /* day of month (1 - 31) */
//  int tm_mon;     /* month of year (0 - 11) */
//  int tm_year;    /* year - 1900 */
//  int tm_wday;    /* day of week (Sunday = 0) */
//  int tm_yday;    /* day of year (0 - 365) */
//  int tm_isdst;   /* is summer time in effect? */
//  char *tm_zone;  /* abbreviation of timezone name */
//  long tm_gmtoff; /* offset from UTC in seconds */
