LLVM_CONFIG   = llvm-config
LLVM_CXXFLAGS = ${shell $(LLVM_CONFIG) --cxxflags}
LLVM_LDFLAGS  = ${shell $(LLVM_CONFIG) --ldflags --libs}

CXX      = llvm-g++
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
            eval.o \
            evlis.o \
            exceptions.o \
            file-io.o \
            heap.o \
            libraries/scheme-base.o \
            libraries/unix-dlopen.o \
            make-closure.o \
            module_import.o \
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

LIB_DEPS =  apply.o \
            arguments.o \
            assertions.o \
            backtrace.o \
            call-lambda.o \
            circular.o \
            cons.o \
            eval.o \
            evlis.o \
            exceptions.o \
            file-io.o \
            libraries/scheme-base.o \
            libraries/unix-dlopen.o \
            make-closure.o \
            module_import.o \
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

libunix-uname.so: libraries/unix-uname.cpp libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) -L. -lmickey $< -o $@

libmickey-environment.so: libraries/mickey-environment.cpp libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) -L. -lmickey $< -o $@

libmickey-internals.so: libraries/mickey-internals.cpp libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) -L. -lmickey $< -o $@

libmickey-misc.so: libraries/mickey-misc.cpp libmickey.so
	$(CXX) -DUSE_LLVM $(CXXFLAGS) $(LLVM_CXXFLAGS) $(LLVM_LDFLAGS) \
		$(LIB_FLAGS) $(LIB_DEPS) -L. -lmickey $< -o $@

libscheme-char.so: libraries/scheme-char.cpp libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) -L. -lmickey  $< -o $@

libscheme-base.so: libraries/scheme-base.cpp libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) -L. -lmickey  $< -o $@

libscheme-load.so: libraries/scheme-load.cpp libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) -L. -lmickey  $< -o $@

libscheme-math.so: libraries/scheme-math.cpp libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) -L. -lmickey  $< -o $@

libscheme-write.so: libraries/scheme-write.cpp libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) -L. -lmickey  $< -o $@

libscheme-repl.so: libraries/scheme-repl.cpp libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) -L. -lmickey  $< -o $@

libscheme-process-context.so: libraries/scheme-process-context.cpp libmickey.so
	$(CXX) $(CXXFLAGS) $(LIB_FLAGS) -L. -lmickey  $< -o $@

mickey: mickey.cpp libmickey.so repl.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) repl.o -L. -lmickey -lreadline $< -o $@

clean:
	rm -rf $(TARGETS) *dSYM
