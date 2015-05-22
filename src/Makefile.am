AM_CPPFLAGS = -I$(top_srcdir)/include
AM_CXXFLAGS = -W -Wall -fno-exceptions -fno-rtti


AM_COLOR_TESTS = no
TEST_LOG_DRIVER = env AM_TAP_AWK='$(AWK)' $(SHELL) \
									$(top_srcdir)/build-aux/tap-driver.sh
TEST_LOG_DRIVER_FLAGS = --comments --ignore-exit
TESTS = \
				begin.test \
				block-comment.test \
				bugs-known.test \
				bugs-regression.test \
				bytevector.test \
				celsius-fahrenheit.test \
				fibonacci.test \
				hofstadter-seq.test \
				let-fxy.test \
				scheme-base.test \
				scheme-char.test \
				scheme-cxr.test \
				scheme-inexact.test \
				scheme-load.test \
				spaced-symbols.test \
				tail-call-elimination.test \
				vector.test

EXTRA_DIST = $(TESTS)

# Note, see
# https://www.gnu.org/software/automake/manual/html_node/Objects-created-both-with-libtool-and-without.html
# Perhaps we could make a library archive, then have mickey executable link
# against that.
#mickey_CXXFLAGS = $(AM_CXXFLAGS)
bin_PROGRAMS = mickey
mickey_LDADD=libmickey.la $(LIBREADLINE)
mickey_SOURCES=mickey.cpp repl.cpp

# ORDER DEPENDENCY: libmickey.la must be first!
pkglib_LTLIBRARIES = \
										 libmickey.la \
										 libc-stdio.la \
										 libeval-cont.la \
										 libimplementation-base.la \
										 libmickey-environment.la \
										 libmickey-gensym.la \
										 libmickey-internals.la \
										 libmickey-misc.la \
										 libposix-dlopen.la \
										 libposix-exit.la \
										 libposix-fork.la \
										 libposix-kill.la \
										 libposix-signal.la \
										 libposix-sysconf.la \
										 libposix-time.la \
										 libposix-uname.la \
										 libposix-unistd.la \
										 libposix-wait.la \
										 libscheme-base.la \
										 libscheme-char.la \
										 libscheme-eval.la \
										 libscheme-file.la \
										 libscheme-load.la \
										 libscheme-math.la \
										 libscheme-process-context.la \
										 libscheme-read.la \
										 libscheme-repl.la \
										 libscheme-time.la \
										 libscheme-write.la \
										 libsrfi-0.la \
										 libsrfi-16.la \
										 libsrfi-4.la

libmickey_la_SOURCES=apply.cpp \
										 arguments.cpp \
										 assertions.cpp \
										 backtrace.cpp \
										 call-lambda.cpp \
										 circular.cpp \
										 cond-expand.cpp \
										 cons.cpp \
										 core-transition.cpp \
										 environment.cpp \
										 eval.cpp \
										 evlis.cpp \
										 exceptions.cpp \
										 file-io.cpp \
										 garbage-collector.cpp \
										 import.cpp \
										 make-closure.cpp \
										 options.cpp \
										 parse-string.cpp \
										 parser-converters.cpp \
										 parser-predicates.cpp \
										 parser.cpp \
										 primitives.cpp \
										 print.cpp \
										 rational_t.cpp \
										 real_t.cpp \
										 strings.cpp \
										 syntax-rules.cpp \
										 system-features.cpp \
										 tokenizer.cpp \
										 util.cpp
libmickey_la_LDFLAGS=-shared -export-dynamic

libscheme_base_la_SOURCES=library/scheme-base.cpp \
													library/scheme-base-math.cpp \
													library/scheme-base-predicates.cpp
libscheme_base_la_LIBADD=libmickey.la
libscheme_base_la_LDFLAGS=-module -shared -export-dynamic

libposix_dlopen_la_SOURCES=library/posix-dlopen.cpp
libposix_dlopen_la_LIBADD=libmickey.la
libposix_dlopen_la_LDFLAGS=-module -shared -export-dynamic

libc_stdio_la_SOURCES=library/c-stdio.cpp
libc_stdio_la_LIBADD=libmickey.la
libc_stdio_la_LDFLAGS=-module -shared -export-dynamic

libmickey_environment_la_SOURCES=library/mickey-environment.cpp
libmickey_environment_la_LIBADD=libmickey.la
libmickey_environment_la_LDFLAGS=-module -shared -export-dynamic

libeval_cont_la_SOURCES=library/eval-cont.cpp
libeval_cont_la_LIBADD=libmickey.la
libeval_cont_la_LDFLAGS=-module -shared -export-dynamic

libimplementation_base_la_SOURCES=library/implementation-base.cpp
libimplementation_base_la_LIBADD=libmickey.la
libimplementation_base_la_LDFLAGS=-module -shared -export-dynamic

libmickey_gensym_la_SOURCES=library/mickey-gensym.cpp
libmickey_gensym_la_LIBADD=libmickey.la
libmickey_gensym_la_LDFLAGS=-module -shared -export-dynamic

libmickey_internals_la_SOURCES=library/mickey-internals.cpp
libmickey_internals_la_LIBADD=libmickey.la
libmickey_internals_la_LDFLAGS=-module -shared -export-dynamic

libmickey_misc_la_SOURCES=library/mickey-misc.cpp
libmickey_misc_la_LIBADD=libmickey.la
libmickey_misc_la_LDFLAGS=-module -shared -export-dynamic

libscheme_char_la_SOURCES=library/scheme-char.cpp
libscheme_char_la_LIBADD=libmickey.la
libscheme_char_la_LDFLAGS=-module -shared -export-dynamic

libscheme_file_la_SOURCES=library/scheme-file.cpp
libscheme_file_la_LIBADD=libmickey.la
libscheme_file_la_LDFLAGS=-module -shared -export-dynamic

libscheme_load_la_SOURCES=library/scheme-load.cpp
libscheme_load_la_LIBADD=libmickey.la
libscheme_load_la_LDFLAGS=-module -shared -export-dynamic

libscheme_math_la_SOURCES=library/scheme-math.cpp
libscheme_math_la_LIBADD=libmickey.la
libscheme_math_la_LDFLAGS=-module -shared -export-dynamic

libscheme_process_context_la_SOURCES=library/scheme-process-context.cpp
libscheme_process_context_la_LIBADD=libmickey.la
libscheme_process_context_la_LDFLAGS=-module -shared -export-dynamic

libscheme_read_la_SOURCES=library/scheme-read.cpp
libscheme_read_la_LIBADD=libmickey.la
libscheme_read_la_LDFLAGS=-module -shared -export-dynamic

libscheme_repl_la_SOURCES=library/scheme-repl.cpp
libscheme_repl_la_LIBADD=libmickey.la
libscheme_repl_la_LDFLAGS=-module -shared -export-dynamic

libscheme_time_la_SOURCES=library/scheme-time.cpp
libscheme_time_la_LIBADD=libmickey.la
libscheme_time_la_LDFLAGS=-module -shared -export-dynamic

libscheme_eval_la_SOURCES=library/scheme-eval.cpp
libscheme_eval_la_LIBADD=libmickey.la
libscheme_eval_la_LDFLAGS=-module -shared -export-dynamic

libscheme_write_la_SOURCES=library/scheme-write.cpp
libscheme_write_la_LIBADD=libmickey.la
libscheme_write_la_LDFLAGS=-module -shared -export-dynamic

libsrfi_0_la_SOURCES=library/srfi-0.cpp
libsrfi_0_la_LIBADD=libmickey.la
libsrfi_0_la_LDFLAGS=-module -shared -export-dynamic

libsrfi_16_la_SOURCES=library/srfi-16.cpp
libsrfi_16_la_LIBADD=libmickey.la
libsrfi_16_la_LDFLAGS=-module -shared -export-dynamic

libsrfi_4_la_SOURCES=library/srfi-4.cpp
libsrfi_4_la_LIBADD=libmickey.la
libsrfi_4_la_LDFLAGS=-module -shared -export-dynamic

libposix_exit_la_SOURCES=library/posix-exit.cpp
libposix_exit_la_LIBADD=libmickey.la
libposix_exit_la_LDFLAGS=-module -shared -export-dynamic

libposix_fork_la_SOURCES=library/posix-fork.cpp
libposix_fork_la_LIBADD=libmickey.la
libposix_fork_la_LDFLAGS=-module -shared -export-dynamic

libposix_kill_la_SOURCES=library/posix-kill.cpp
libposix_kill_la_LIBADD=libmickey.la
libposix_kill_la_LDFLAGS=-module -shared -export-dynamic

libposix_signal_la_SOURCES=library/posix-signal.cpp
libposix_signal_la_LIBADD=libmickey.la
libposix_signal_la_LDFLAGS=-module -shared -export-dynamic

libposix_sysconf_la_SOURCES=library/posix-sysconf.cpp
libposix_sysconf_la_LIBADD=libmickey.la
libposix_sysconf_la_LDFLAGS=-module -shared -export-dynamic

libposix_time_la_SOURCES=library/posix-time.cpp
libposix_time_la_LIBADD=libmickey.la
libposix_time_la_LDFLAGS=-module -shared -export-dynamic

libposix_uname_la_SOURCES=library/posix-uname.cpp
libposix_uname_la_LIBADD=libmickey.la
libposix_uname_la_LDFLAGS=-module -shared -export-dynamic

libposix_unistd_la_SOURCES=library/posix-unistd.cpp
libposix_unistd_la_LIBADD=libmickey.la
libposix_unistd_la_LDFLAGS=-module -shared -export-dynamic

libposix_wait_la_SOURCES=library/posix-wait.cpp
libposix_wait_la_LIBADD=libmickey.la
libposix_wait_la_LDFLAGS=-module -shared -export-dynamic
