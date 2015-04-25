#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([mickey], [0.1])
AC_CONFIG_SRCDIR([src/mickey.cpp])
AC_CONFIG_HEADERS([include/config.h])
AC_CONFIG_MACRO_DIRS([m4])
AM_INIT_AUTOMAKE([subdir-objects])
LT_INIT

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_MAKE_SET

# Checks for libraries.
AC_CHECK_LIB([dl], [dlopen])
AC_CHECK_LIB([ffi], [ffi_call])
AC_CHECK_LIB([readline], [readline])

# Checks for header files.
AC_CHECK_HEADERS([inttypes.h limits.h stdint.h stdlib.h string.h sys/param.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_INT8_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_STRUCT_TIMEZONE
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T

# Checks for library functions.
AC_FUNC_FORK
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([floor getcwd gethostname localtime_r memset realpath sqrt strchr strdup strerror strtol uname])

AC_CONFIG_FILES([Makefile
                 src/Makefile])

# TODO: Add these using --with-.... flags
# lib/optional/Makefile
# lib/optional/gnupg/Makefile
# lib/optional/libffi/Makefile

AC_OUTPUT
