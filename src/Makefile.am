AM_CPPFLAGS = -I$(top_srcdir)/include

bin_PROGRAMS=mickey
# Note, see
# https://www.gnu.org/software/automake/manual/html_node/Objects-created-both-with-libtool-and-without.html
# Perhaps we could make a library archive, then have mickey executable link
# against that.
#mickey_CXXFLAGS = $(AM_CXXFLAGS)
mickey_LDADD=libmickey.la
mickey_SOURCES=mickey.cpp repl.cpp

lib_LTLIBRARIES=libmickey.la \
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

libscheme_base_la_SOURCES=scheme-base.cpp \
													scheme-base-math.cpp \
													scheme-base-predicates.cpp
libscheme_base_la_LIBADD=libmickey.la

libposix_dlopen_la_SOURCES=posix-dlopen.cpp
libposix_dlopen_la_LIBADD=libmickey.la

libc_stdio_la_SOURCES=c-stdio.cpp
libc_stdio_la_LIBADD=libmickey.la

libmickey_environment_la_SOURCES=mickey-environment.cpp
libmickey_environment_la_LIBADD=libmickey.la

libeval_cont_la_SOURCES=eval-cont.cpp
libeval_cont_la_LIBADD=libmickey.la

libimplementation_base_la_SOURCES=implementation-base.cpp
libimplementation_base_la_LIBADD=libmickey.la

libmickey_gensym_la_SOURCES=mickey-gensym.cpp
libmickey_gensym_la_LIBADD=libmickey.la

libmickey_internals_la_SOURCES=mickey-internals.cpp
libmickey_internals_la_LIBADD=libmickey.la

libmickey_misc_la_SOURCES=mickey-misc.cpp
libmickey_misc_la_LIBADD=libmickey.la

libscheme_char_la_SOURCES=scheme-char.cpp
libscheme_char_la_LIBADD=libmickey.la

libscheme_file_la_SOURCES=scheme-file.cpp
libscheme_file_la_LIBADD=libmickey.la

libscheme_load_la_SOURCES=scheme-load.cpp
libscheme_load_la_LIBADD=libmickey.la

libscheme_math_la_SOURCES=scheme-math.cpp
libscheme_math_la_LIBADD=libmickey.la

libscheme_process_context_la_SOURCES=scheme-process-context.cpp
libscheme_process_context_la_LIBADD=libmickey.la

libscheme_read_la_SOURCES=scheme-read.cpp
libscheme_read_la_LIBADD=libmickey.la

libscheme_repl_la_SOURCES=scheme-repl.cpp
libscheme_repl_la_LIBADD=libmickey.la

libscheme_time_la_SOURCES=scheme-time.cpp
libscheme_time_la_LIBADD=libmickey.la

libscheme_eval_la_SOURCES=scheme-eval.cpp
libscheme_eval_la_LIBADD=libmickey.la

libscheme_write_la_SOURCES=scheme-write.cpp
libscheme_write_la_LIBADD=libmickey.la

libsrfi_0_la_SOURCES=srfi-0.cpp
libsrfi_0_la_LIBADD=libmickey.la

libsrfi_16_la_SOURCES=srfi-16.cpp
libsrfi_16_la_LIBADD=libmickey.la

libsrfi_4_la_SOURCES=srfi-4.cpp
libsrfi_4_la_LIBADD=libmickey.la

libposix_exit_la_SOURCES=posix-exit.cpp
libposix_exit_la_LIBADD=libmickey.la

libposix_fork_la_SOURCES=posix-fork.cpp
libposix_fork_la_LIBADD=libmickey.la

libposix_kill_la_SOURCES=posix-kill.cpp
libposix_kill_la_LIBADD=libmickey.la

libposix_signal_la_SOURCES=posix-signal.cpp
libposix_signal_la_LIBADD=libmickey.la

libposix_sysconf_la_SOURCES=posix-sysconf.cpp
libposix_sysconf_la_LIBADD=libmickey.la

libposix_time_la_SOURCES=posix-time.cpp
libposix_time_la_LIBADD=libmickey.la

libposix_uname_la_SOURCES=posix-uname.cpp
libposix_uname_la_LIBADD=libmickey.la

libposix_unistd_la_SOURCES=posix-unistd.cpp
libposix_unistd_la_LIBADD=libmickey.la

libposix_wait_la_SOURCES=posix-wait.cpp
libposix_wait_la_LIBADD=libmickey.la
