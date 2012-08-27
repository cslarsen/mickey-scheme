CXX      = c++
CXXFLAGS = -g -W -Wall \
					 -I../include \
					 -DUSE_READLINE \
					 -fno-exceptions \
					 -fno-rtti

LDFLAGS  =
LIB_FLAGS = -shared -fPIC $(LDFLAGS)

TARGETS_O =  \
            apply.o \
            arguments.o \
            assertions.o \
            backtrace.o \
            call-lambda.o \
            circular.o \
            cons.o \
            core-transition.o \
            eval.o \
            evlis.o \
            exceptions.o \
            file-io.o \
            heap.o \
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
            eval.o \
            evlis.o \
            exceptions.o \
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

TARGETS_SO = libunix-uname.so \
						 libunix-dlopen.so \
						 libmickey-misc.so \
						 libmickey-environment.so \
						 libmickey-internals.so \
						 libscheme-char.so \
						 libscheme-base.so \
						 libscheme-load.so \
						 libscheme-write.so \
						 libscheme-repl.so \
						 libscheme-math.so \
						 libscheme-process-context.so

TARGETS = $(TARGETS_O) $(TARGETS_SO) libmickey.so mickey

all: $(TARGETS)

libmickey.so: $(LIB_DEPS)
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) $^ -L. -o $@

lib%.so: libraries/%.cpp libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) -L. -lmickey $< -o $@

mickey: mickey.cpp libmickey.so repl.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) repl.o -L. -lmickey -lreadline $< -o $@

clean:
	rm -rf $(TARGETS) *dSYM
