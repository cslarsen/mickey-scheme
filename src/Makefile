CXX      = g++
CXXFLAGS = -g -W -Wall \
					 -fPIC \
					 -I../include \
					 -DDEBUG \
					 -DUSE_READLINE \
					 -fno-exceptions \
					 -fno-rtti

LDFLAGS  =
LIB_FLAGS = -shared -fpic $(LDFLAGS)

TARGETS_O =  \
            apply.o \
            arguments.o \
            assertions.o \
            backtrace.o \
            call-lambda.o \
            circular.o \
            cond-expand.o \
            cons.o \
            core-transition.o \
            environment.o \
            eval.o \
            evlis.o \
            exceptions.o \
            file-io.o \
            import.o \
            make-closure.o \
            options.o \
            parse-string.o \
            parser-converters.o \
            parser-predicates.o \
            parser.o \
            primitives.o \
            print.o \
            rational_t.o \
            real_t.o \
            repl.o \
            strings.o \
            syntax-rules.o \
            system-features.o \
            tokenizer.o \
            util.o

TARGETS_LIB_O = \
								library/scheme-base-math.o \
								library/scheme-base-predicates.o \
								library/scheme-base.o
LIB_DEPS =  \
            apply.o \
            arguments.o \
            assertions.o \
            backtrace.o \
            call-lambda.o \
            circular.o \
            cond-expand.o \
            cons.o \
            core-transition.o \
            environment.o \
            eval.o \
            evlis.o \
            exceptions.o \
            file-io.o \
            import.o \
            make-closure.o \
            options.o \
            parse-string.o \
            parser-converters.o \
            parser-predicates.o \
            parser.o \
            primitives.o \
            print.o \
            rational_t.o \
            real_t.o \
            strings.o \
            syntax-rules.o \
            system-features.o \
            tokenizer.o \
            util.o

TARGETS_SO = \
						 libc-stdio.so \
						 libmickey-environment.so \
						 libeval-cont.so \
						 libimplementation-base.so \
						 libmickey-gensym.so \
						 libmickey-internals.so \
						 libmickey-misc.so \
						 libscheme-base.so \
						 libscheme-char.so \
						 libscheme-file.so \
						 libscheme-load.so \
						 libscheme-math.so \
						 libscheme-process-context.so \
						 libscheme-read.so \
						 libscheme-repl.so \
						 libscheme-time.so \
						 libscheme-eval.so \
						 libscheme-write.so \
						 libsrfi-0.so \
						 libsrfi-16.so \
						 libsrfi-4.so \
						 libunix-dlopen.so \
						 libunix-exit.so \
						 libunix-fork.so \
						 libunix-kill.so \
						 libunix-signal.so \
						 libunix-uname.so \
						 libunix-unistd.so \
						 libunix-time.so \
						 libunix-wait.so

TARGETS = $(TARGETS_O) $(TARGETS_SO) $(TARGETS_LIB_O) libmickey.so mickey

all: $(TARGETS)

libmickey.so: $(LIB_DEPS)
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) $^ -L. -ldl -o $@

lib%.so: library/%.cpp libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) -L. -lmickey $< -o $@

libcross-platform-sdl.so: library/cross-platform-sdl.cpp libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) $(shell sdl-config --cflags --libs) -L. -lmickey $< -o $@

libscheme-base.so: library/scheme-base.o library/scheme-base-math.o library/scheme-base-predicates.o libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) -L. -lmickey \
		library/scheme-base.o \
		library/scheme-base-math.o \
		library/scheme-base-predicates.o -o $@

mickey: mickey.cpp libmickey.so repl.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) repl.o -L. -lmickey -lreadline $< -o $@

clean:
	rm -rf $(TARGETS) *dSYM
