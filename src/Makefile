CXX      = c++
CXXFLAGS = -g -W -Wall \
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
            cons.o \
            core-transition.o \
            environment.o \
            eval.o \
            evlis.o \
            exceptions.o \
            features.o \
            file-io.o \
            import.o \
            make-closure.o \
            options.o \
            parse-string.o \
            parser.o \
            primitives.o \
            print.o \
            repl.o \
            syntax-rules.o \
            tokenizer.o \
            types.o \
            util.o \

LIB_DEPS =  \
            apply.o \
            arguments.o \
            assertions.o \
            backtrace.o \
            call-lambda.o \
            circular.o \
            cons.o \
            core-transition.o \
            environment.o \
            eval.o \
            evlis.o \
            exceptions.o \
            features.o \
            file-io.o \
            import.o \
            make-closure.o \
            options.o \
            parse-string.o \
            parser.o \
            primitives.o \
            print.o \
            syntax-rules.o \
            tokenizer.o \
            types.o \
            util.o

TARGETS_SO = \
						 libc-stdio.so \
						 libmickey-environment.so \
						 libmickey-internals.so \
						 libmickey-misc.so \
						 libcross-platform-sdl.so \
						 libscheme-base.so \
						 libscheme-char.so \
						 libscheme-load.so \
						 libscheme-math.so \
						 libscheme-process-context.so \
						 libscheme-repl.so \
						 libscheme-write.so \
						 libunix-dlopen.so \
						 libunix-uname.so \

TARGETS = $(TARGETS_O) $(TARGETS_SO) libmickey.so mickey

all: $(TARGETS)

libmickey.so: $(LIB_DEPS)
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) $^ -L. -o $@

lib%.so: library/%.cpp libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) -L. -lmickey $< -o $@

libcross-platform-sdl.so: library/cross-platform-sdl.cpp libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) $(shell sdl-config --cflags --libs) -L. -lmickey $< -o $@

mickey: mickey.cpp libmickey.so repl.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) repl.o -L. -lmickey -lreadline $< -o $@

clean:
	rm -rf $(TARGETS) *dSYM
