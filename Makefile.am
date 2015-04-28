ACLOCAL_AMFLAGS=-I m4
EXTRA_DIST = autogen.sh lib
SUBDIRS = src

pkginclude_HEADERS = \
										 include/apply.h \
										 include/arguments.h \
										 include/assertions.h \
										 include/backtrace.h \
										 include/circular.h \
										 include/cons.h \
										 include/debug.h \
										 include/eval.h \
										 include/evlis.h \
										 include/exceptions.h \
										 include/file-io.h \
										 include/import.h \
										 include/mickey.h \
										 include/options.h \
										 include/parser.h \
										 include/primitives.h \
										 include/print.h \
										 include/syntax-rules.h \
										 include/tokenizer.h \
										 include/util.h

include library.m4

AM_COLOR_TESTS=no
TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = $(prefix)/bin/mickey
AM_SCM_LOG_FLAGS = -Itests
TESTS = \
				tests/programs/celsius-fahrenheit.scm \
				tests/programs/fibonacci.scm \
				tests/programs/tail-call-elimination.scm \
				tests/test-scheme-base.scm \
				tests/test-scheme-char.scm \
				tests/test-scheme-cxr.scm \
				tests/test-scheme-inexact.scm \
				tests/test-scheme-load.scm
