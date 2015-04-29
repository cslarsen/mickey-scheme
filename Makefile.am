ACLOCAL_AMFLAGS=-I m4 --install
EXTRA_DIST = autogen.sh lib include tests m4/.dummy
SUBDIRS = src

pkginclude_HEADERS = \
										 include/mickey/apply.h \
										 include/mickey/arguments.h \
										 include/mickey/assertions.h \
										 include/mickey/backtrace.h \
										 include/mickey/circular.h \
										 include/mickey/cons.h \
										 include/mickey/debug.h \
										 include/mickey/eval.h \
										 include/mickey/evlis.h \
										 include/mickey/exceptions.h \
										 include/mickey/file-io.h \
										 include/mickey/import.h \
										 include/mickey/mickey.h \
										 include/mickey/options.h \
										 include/mickey/parser.h \
										 include/mickey/primitives.h \
										 include/mickey/print.h \
										 include/mickey/syntax-rules.h \
										 include/mickey/tokenizer.h \
										 include/mickey/util.h

include library.m4

AM_COLOR_TESTS = no
TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = src/mickey -L$(srcdir)/lib -Lsrc/.libs
AM_SCM_LOG_FLAGS = -Itests
TESTS = \
				tests/programs/bytevector.scm \
				tests/programs/celsius-fahrenheit.scm \
				tests/programs/fibonacci.scm \
				tests/programs/hofstadter-seq.scm \
				tests/programs/let-fxy.scm \
				tests/programs/tail-call-elimination.scm \
				tests/test-scheme-base.scm \
				tests/test-scheme-char.scm \
				tests/test-scheme-cxr.scm \
				tests/test-scheme-inexact.scm \
				tests/test-scheme-load.scm
