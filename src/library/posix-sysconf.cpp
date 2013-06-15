/*
 * UNIX wait for Mickey Scheme
 *
 * Copyright (C) 2013 Christian Stigen Larsen
 * Distributed under the LGPL 2.1; see LICENSE
 */

#include <unistd.h>
#include "mickey.h"

typedef struct {
  const char* name;
  int value;
} _var;

static _var vars[] = {

#ifdef _SC_2_CHAR_TERM
  {"_SC_2_CHAR_TERM", _SC_2_CHAR_TERM},
#endif

#ifdef _SC_2_C_BIND
  {"_SC_2_C_BIND", _SC_2_C_BIND},
#endif

#ifdef _SC_2_C_DEV
  {"_SC_2_C_DEV", _SC_2_C_DEV},
#endif

#ifdef _SC_2_FORT_DEV
  {"_SC_2_FORT_DEV", _SC_2_FORT_DEV},
#endif

#ifdef _SC_2_FORT_RUN
  {"_SC_2_FORT_RUN", _SC_2_FORT_RUN},
#endif

#ifdef _SC_2_LOCALEDEF
  {"_SC_2_LOCALEDEF", _SC_2_LOCALEDEF},
#endif

#ifdef _SC_2_PBS
  {"_SC_2_PBS", _SC_2_PBS},
#endif

#ifdef _SC_2_PBS_ACCOUNTING
  {"_SC_2_PBS_ACCOUNTING", _SC_2_PBS_ACCOUNTING},
#endif

#ifdef _SC_2_PBS_CHECKPOINT
  {"_SC_2_PBS_CHECKPOINT", _SC_2_PBS_CHECKPOINT},
#endif

#ifdef _SC_2_PBS_LOCATE
  {"_SC_2_PBS_LOCATE", _SC_2_PBS_LOCATE},
#endif

#ifdef _SC_2_PBS_MESSAGE
  {"_SC_2_PBS_MESSAGE", _SC_2_PBS_MESSAGE},
#endif

#ifdef _SC_2_PBS_TRACK
  {"_SC_2_PBS_TRACK", _SC_2_PBS_TRACK},
#endif

#ifdef _SC_2_SW_DEV
  {"_SC_2_SW_DEV", _SC_2_SW_DEV},
#endif

#ifdef _SC_2_UPE
  {"_SC_2_UPE", _SC_2_UPE},
#endif

#ifdef _SC_2_VERSION
  {"_SC_2_VERSION", _SC_2_VERSION},
#endif

#ifdef _SC_ADVISORY_INFO
  {"_SC_ADVISORY_INFO", _SC_ADVISORY_INFO},
#endif

#ifdef _SC_AIO_LISTIO_MAX
  {"_SC_AIO_LISTIO_MAX", _SC_AIO_LISTIO_MAX},
#endif

#ifdef _SC_AIO_MAX
  {"_SC_AIO_MAX", _SC_AIO_MAX},
#endif

#ifdef _SC_AIO_PRIO_DELTA_MAX
  {"_SC_AIO_PRIO_DELTA_MAX", _SC_AIO_PRIO_DELTA_MAX},
#endif

#ifdef _SC_ARG_MAX
  {"_SC_ARG_MAX", _SC_ARG_MAX},
#endif

#ifdef _SC_ASYNCHRONOUS_IO
  {"_SC_ASYNCHRONOUS_IO", _SC_ASYNCHRONOUS_IO},
#endif

#ifdef _SC_ATEXIT_MAX
  {"_SC_ATEXIT_MAX", _SC_ATEXIT_MAX},
#endif

#ifdef _SC_BARRIERS
  {"_SC_BARRIERS", _SC_BARRIERS},
#endif

#ifdef _SC_BC_BASE_MAX
  {"_SC_BC_BASE_MAX", _SC_BC_BASE_MAX},
#endif

#ifdef _SC_BC_DIM_MAX
  {"_SC_BC_DIM_MAX", _SC_BC_DIM_MAX},
#endif

#ifdef _SC_BC_SCALE_MAX
  {"_SC_BC_SCALE_MAX", _SC_BC_SCALE_MAX},
#endif

#ifdef _SC_BC_STRING_MAX
  {"_SC_BC_STRING_MAX", _SC_BC_STRING_MAX},
#endif

#ifdef _SC_CHILD_MAX
  {"_SC_CHILD_MAX", _SC_CHILD_MAX},
#endif

#ifdef _SC_CLK_TCK
  {"_SC_CLK_TCK", _SC_CLK_TCK},
#endif

#ifdef _SC_CLOCK_SELECTION
  {"_SC_CLOCK_SELECTION", _SC_CLOCK_SELECTION},
#endif

#ifdef _SC_COLL_WEIGHTS_MAX
  {"_SC_COLL_WEIGHTS_MAX", _SC_COLL_WEIGHTS_MAX},
#endif

#ifdef _SC_CPUTIME
  {"_SC_CPUTIME", _SC_CPUTIME},
#endif

#ifdef _SC_DELAYTIMER_MAX
  {"_SC_DELAYTIMER_MAX", _SC_DELAYTIMER_MAX},
#endif

#ifdef _SC_EXPR_NEST_MAX
  {"_SC_EXPR_NEST_MAX", _SC_EXPR_NEST_MAX},
#endif

#ifdef _SC_FSYNC
  {"_SC_FSYNC", _SC_FSYNC},
#endif

#ifdef _SC_GETGR_R_SIZE_MAX
  {"_SC_GETGR_R_SIZE_MAX", _SC_GETGR_R_SIZE_MAX},
#endif

#ifdef _SC_GETPW_R_SIZE_MAX
  {"_SC_GETPW_R_SIZE_MAX", _SC_GETPW_R_SIZE_MAX},
#endif

#ifdef _SC_HOST_NAME_MAX
  {"_SC_HOST_NAME_MAX", _SC_HOST_NAME_MAX},
#endif

#ifdef _SC_IOV_MAX
  {"_SC_IOV_MAX", _SC_IOV_MAX},
#endif

#ifdef _SC_IPV6
  {"_SC_IPV6", _SC_IPV6},
#endif

#ifdef _SC_JOB_CONTROL
  {"_SC_JOB_CONTROL", _SC_JOB_CONTROL},
#endif

#ifdef _SC_LINE_MAX
  {"_SC_LINE_MAX", _SC_LINE_MAX},
#endif

#ifdef _SC_LOGIN_NAME_MAX
  {"_SC_LOGIN_NAME_MAX", _SC_LOGIN_NAME_MAX},
#endif

#ifdef _SC_MAPPED_FILES
  {"_SC_MAPPED_FILES", _SC_MAPPED_FILES},
#endif

#ifdef _SC_MEMLOCK
  {"_SC_MEMLOCK", _SC_MEMLOCK},
#endif

#ifdef _SC_MEMLOCK_RANGE
  {"_SC_MEMLOCK_RANGE", _SC_MEMLOCK_RANGE},
#endif

#ifdef _SC_MEMORY_PROTECTION
  {"_SC_MEMORY_PROTECTION", _SC_MEMORY_PROTECTION},
#endif

#ifdef _SC_MESSAGE_PASSING
  {"_SC_MESSAGE_PASSING", _SC_MESSAGE_PASSING},
#endif

#ifdef _SC_MONOTONIC_CLOCK
  {"_SC_MONOTONIC_CLOCK", _SC_MONOTONIC_CLOCK},
#endif

#ifdef _SC_MQ_OPEN_MAX
  {"_SC_MQ_OPEN_MAX", _SC_MQ_OPEN_MAX},
#endif

#ifdef _SC_MQ_PRIO_MAX
  {"_SC_MQ_PRIO_MAX", _SC_MQ_PRIO_MAX},
#endif

#ifdef _SC_NGROUPS_MAX
  {"_SC_NGROUPS_MAX", _SC_NGROUPS_MAX},
#endif

#ifdef _SC_OPEN_MAX
  {"_SC_OPEN_MAX", _SC_OPEN_MAX},
#endif

#ifdef _SC_PAGESIZE
  {"_SC_PAGESIZE", _SC_PAGESIZE},
#endif

#ifdef _SC_PAGE_SIZE
  {"_SC_PAGE_SIZE", _SC_PAGE_SIZE},
#endif

#ifdef _SC_PRIORITIZED_IO
  {"_SC_PRIORITIZED_IO", _SC_PRIORITIZED_IO},
#endif

#ifdef _SC_PRIORITY_SCHEDULING
  {"_SC_PRIORITY_SCHEDULING", _SC_PRIORITY_SCHEDULING},
#endif

#ifdef _SC_RAW_SOCKETS
  {"_SC_RAW_SOCKETS", _SC_RAW_SOCKETS},
#endif

#ifdef _SC_READER_WRITER_LOCKS
  {"_SC_READER_WRITER_LOCKS", _SC_READER_WRITER_LOCKS},
#endif

#ifdef _SC_REALTIME_SIGNALS
  {"_SC_REALTIME_SIGNALS", _SC_REALTIME_SIGNALS},
#endif

#ifdef _SC_REGEXP
  {"_SC_REGEXP", _SC_REGEXP},
#endif

#ifdef _SC_RE_DUP_MAX
  {"_SC_RE_DUP_MAX", _SC_RE_DUP_MAX},
#endif

#ifdef _SC_RTSIG_MAX
  {"_SC_RTSIG_MAX", _SC_RTSIG_MAX},
#endif

#ifdef _SC_SAVED_IDS
  {"_SC_SAVED_IDS", _SC_SAVED_IDS},
#endif

#ifdef _SC_SEMAPHORES
  {"_SC_SEMAPHORES", _SC_SEMAPHORES},
#endif

#ifdef _SC_SEM_NSEMS_MAX
  {"_SC_SEM_NSEMS_MAX", _SC_SEM_NSEMS_MAX},
#endif

#ifdef _SC_SEM_VALUE_MAX
  {"_SC_SEM_VALUE_MAX", _SC_SEM_VALUE_MAX},
#endif

#ifdef _SC_SHARED_MEMORY_OBJECTS
  {"_SC_SHARED_MEMORY_OBJECTS", _SC_SHARED_MEMORY_OBJECTS},
#endif

#ifdef _SC_SHELL
  {"_SC_SHELL", _SC_SHELL},
#endif

#ifdef _SC_SIGQUEUE_MAX
  {"_SC_SIGQUEUE_MAX", _SC_SIGQUEUE_MAX},
#endif

#ifdef _SC_SPAWN
  {"_SC_SPAWN", _SC_SPAWN},
#endif

#ifdef _SC_SPIN_LOCKS
  {"_SC_SPIN_LOCKS", _SC_SPIN_LOCKS},
#endif

#ifdef _SC_SPORADIC_SERVER
  {"_SC_SPORADIC_SERVER", _SC_SPORADIC_SERVER},
#endif

#ifdef _SC_SS_REPL_MAX
  {"_SC_SS_REPL_MAX", _SC_SS_REPL_MAX},
#endif

#ifdef _SC_STREAM_MAX
  {"_SC_STREAM_MAX", _SC_STREAM_MAX},
#endif

#ifdef _SC_SYMLOOP_MAX
  {"_SC_SYMLOOP_MAX", _SC_SYMLOOP_MAX},
#endif

#ifdef _SC_SYNCHRONIZED_IO
  {"_SC_SYNCHRONIZED_IO", _SC_SYNCHRONIZED_IO},
#endif

#ifdef _SC_THREADS
  {"_SC_THREADS", _SC_THREADS},
#endif

#ifdef _SC_THREAD_ATTR_STACKADDR
  {"_SC_THREAD_ATTR_STACKADDR", _SC_THREAD_ATTR_STACKADDR},
#endif

#ifdef _SC_THREAD_ATTR_STACKSIZE
  {"_SC_THREAD_ATTR_STACKSIZE", _SC_THREAD_ATTR_STACKSIZE},
#endif

#ifdef _SC_THREAD_CPUTIME
  {"_SC_THREAD_CPUTIME", _SC_THREAD_CPUTIME},
#endif

#ifdef _SC_THREAD_DESTRUCTOR_ITERATIONS
  {"_SC_THREAD_DESTRUCTOR_ITERATIONS", _SC_THREAD_DESTRUCTOR_ITERATIONS},
#endif

#ifdef _SC_THREAD_KEYS_MAX
  {"_SC_THREAD_KEYS_MAX", _SC_THREAD_KEYS_MAX},
#endif

#ifdef _SC_THREAD_PRIORITY_SCHEDULING
  {"_SC_THREAD_PRIORITY_SCHEDULING", _SC_THREAD_PRIORITY_SCHEDULING},
#endif

#ifdef _SC_THREAD_PRIO_INHERIT
  {"_SC_THREAD_PRIO_INHERIT", _SC_THREAD_PRIO_INHERIT},
#endif

#ifdef _SC_THREAD_PRIO_PROTECT
  {"_SC_THREAD_PRIO_PROTECT", _SC_THREAD_PRIO_PROTECT},
#endif

#ifdef _SC_THREAD_PROCESS_SHARED
  {"_SC_THREAD_PROCESS_SHARED", _SC_THREAD_PROCESS_SHARED},
#endif

#ifdef _SC_THREAD_ROBUST_PRIO_INHERIT
  {"_SC_THREAD_ROBUST_PRIO_INHERIT", _SC_THREAD_ROBUST_PRIO_INHERIT},
#endif

#ifdef _SC_THREAD_ROBUST_PRIO_PROTECT
  {"_SC_THREAD_ROBUST_PRIO_PROTECT", _SC_THREAD_ROBUST_PRIO_PROTECT},
#endif

#ifdef _SC_THREAD_SAFE_FUNCTIONS
  {"_SC_THREAD_SAFE_FUNCTIONS", _SC_THREAD_SAFE_FUNCTIONS},
#endif

#ifdef _SC_THREAD_SPORADIC_SERVER
  {"_SC_THREAD_SPORADIC_SERVER", _SC_THREAD_SPORADIC_SERVER},
#endif

#ifdef _SC_THREAD_STACK_MIN
  {"_SC_THREAD_STACK_MIN", _SC_THREAD_STACK_MIN},
#endif

#ifdef _SC_THREAD_THREADS_MAX
  {"_SC_THREAD_THREADS_MAX", _SC_THREAD_THREADS_MAX},
#endif

#ifdef _SC_TIMEOUTS
  {"_SC_TIMEOUTS", _SC_TIMEOUTS},
#endif

#ifdef _SC_TIMERS
  {"_SC_TIMERS", _SC_TIMERS},
#endif

#ifdef _SC_TIMER_MAX
  {"_SC_TIMER_MAX", _SC_TIMER_MAX},
#endif

#ifdef _SC_TRACE
  {"_SC_TRACE", _SC_TRACE},
#endif

#ifdef _SC_TRACE_EVENT_FILTER
  {"_SC_TRACE_EVENT_FILTER", _SC_TRACE_EVENT_FILTER},
#endif

#ifdef _SC_TRACE_EVENT_NAME_MAX
  {"_SC_TRACE_EVENT_NAME_MAX", _SC_TRACE_EVENT_NAME_MAX},
#endif

#ifdef _SC_TRACE_INHERIT
  {"_SC_TRACE_INHERIT", _SC_TRACE_INHERIT},
#endif

#ifdef _SC_TRACE_LOG
  {"_SC_TRACE_LOG", _SC_TRACE_LOG},
#endif

#ifdef _SC_TRACE_NAME_MAX
  {"_SC_TRACE_NAME_MAX", _SC_TRACE_NAME_MAX},
#endif

#ifdef _SC_TRACE_SYS_MAX
  {"_SC_TRACE_SYS_MAX", _SC_TRACE_SYS_MAX},
#endif

#ifdef _SC_TRACE_USER_EVENT_MAX
  {"_SC_TRACE_USER_EVENT_MAX", _SC_TRACE_USER_EVENT_MAX},
#endif

#ifdef _SC_TTY_NAME_MAX
  {"_SC_TTY_NAME_MAX", _SC_TTY_NAME_MAX},
#endif

#ifdef _SC_TYPED_MEMORY_OBJECTS
  {"_SC_TYPED_MEMORY_OBJECTS", _SC_TYPED_MEMORY_OBJECTS},
#endif

#ifdef _SC_TZNAME_MAX
  {"_SC_TZNAME_MAX", _SC_TZNAME_MAX},
#endif

#ifdef _SC_V6_ILP32_OFFBIG
  {"_SC_V6_ILP32_OFFBIG", _SC_V6_ILP32_OFFBIG},
#endif

#ifdef _SC_V6_LP64_OFF64
  {"_SC_V6_LP64_OFF64", _SC_V6_LP64_OFF64},
#endif

#ifdef _SC_V6_LPBIG_OFFBIG 
  {"_SC_V6_LPBIG_OFFBIG ", _SC_V6_LPBIG_OFFBIG },
#endif

#ifdef _SC_V7_ILP32_OFF32
  {"_SC_V7_ILP32_OFF32", _SC_V7_ILP32_OFF32},
#endif

#ifdef _SC_V7_ILP32_OFFBIG
  {"_SC_V7_ILP32_OFFBIG", _SC_V7_ILP32_OFFBIG},
#endif

#ifdef _SC_V7_LP64_OFF64
  {"_SC_V7_LP64_OFF64", _SC_V7_LP64_OFF64},
#endif

#ifdef _SC_V7_LPBIG_OFFBIG
  {"_SC_V7_LPBIG_OFFBIG", _SC_V7_LPBIG_OFFBIG},
#endif

#ifdef _SC_VERSION
  {"_SC_VERSION", _SC_VERSION},
#endif

#ifdef _SC_XOPEN_CRYPT
  {"_SC_XOPEN_CRYPT", _SC_XOPEN_CRYPT},
#endif

#ifdef _SC_XOPEN_ENH_I18N
  {"_SC_XOPEN_ENH_I18N", _SC_XOPEN_ENH_I18N},
#endif

#ifdef _SC_XOPEN_REALTIME
  {"_SC_XOPEN_REALTIME", _SC_XOPEN_REALTIME},
#endif

#ifdef _SC_XOPEN_REALTIME_THREADS
  {"_SC_XOPEN_REALTIME_THREADS", _SC_XOPEN_REALTIME_THREADS},
#endif

#ifdef _SC_XOPEN_SHM
  {"_SC_XOPEN_SHM", _SC_XOPEN_SHM},
#endif

#ifdef _SC_XOPEN_STREAMS
  {"_SC_XOPEN_STREAMS", _SC_XOPEN_STREAMS},
#endif

#ifdef _SC_XOPEN_UNIX
  {"_SC_XOPEN_UNIX", _SC_XOPEN_UNIX},
#endif

#ifdef _SC_XOPEN_UUCP
  {"_SC_XOPEN_UUCP", _SC_XOPEN_UUCP},
#endif

#ifdef _SC_XOPEN_VERSION
  {"_SC_XOPEN_VERSION", _SC_XOPEN_VERSION},
#endif

#ifdef _SC_NPROCESSORS_CONF
  {"_SC_NPROCESSORS_CONF", _SC_NPROCESSORS_CONF},
#endif

#ifdef _SC_NPROCESSORS_ONLN
  {"_SC_NPROCESSORS_ONLN", _SC_NPROCESSORS_ONLN},
#endif

#ifdef SC_PHYS_PAGES
  {"SC_PHYS_PAGES", SC_PHYS_PAGES},
#endif

  {NULL, 0}
};

static int lookup(const char* name)
{
  for ( int n=0; vars[n].name != NULL; ++n )
    if ( !strcmp(name, vars[n].name) )
      return vars[n].value;
  return -1;
}

extern "C" cons_t* proc_sysconf(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(SYMBOL, car(p));
  int name = lookup(symbol_name(car(p)).c_str());

  // symbol not recognized?
  if ( name < 0 )
    return boolean(false);

  int ret = sysconf(name);

  // name not recognized?
  if ( ret == -1 )
    return boolean(false);

  return integer(ret);
}

extern "C" cons_t* proc_sysconf_symbols(cons_t* p, environment_t*)
{
  assert_length(p, 0);

  cons_t *r = list();

  for ( int n=(sizeof(vars)/sizeof(_var))-1; n>=0; --n )
    if ( vars[n].name != NULL )
      r = cons(symbol(vars[n].name), r);

  return r;
}
