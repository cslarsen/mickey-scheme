TARGETS = ffi.o libffi.so
CXX = /usr/bin/g++
MICKEY = ../../..

LIBFFI_CFLAGS = #-I/usr/include
LIBFFI_LIBS = -lffi

CXXFLAGS = -W -Wall -I$(MICKEY)/include $(LIBFFI_CFLAGS)
LDFLAGS = -L$(MICKEY)/lib -lmickey $(LIBFFI_LIBS)
LIBFLAGS = -shared -fpic

MICKEY_LIB_PATH = DYLD_LIBRARY_PATH=$(MICKEY) LD_LIBRARY_PATH=$(MICKEY)

all: $(TARGETS)

libffi.so: ffi.o

lib%.so: %.o
	$(CXX) $(CXXFLAGS) $(LIBFLAGS) $(LDFLAGS) $< -o $@

check: all
	@$(MICKEY_LIB_PATH) $(MICKEY)/mickey -L. tests.scm

install: check
	@mkdir -p $(MICKEY)/lib/ffi
	@cp -f libffi.scm $(MICKEY)/lib/ffi
	@cp -f libffi.so $(MICKEY)/lib/
	@echo ""
	@echo "You need to manually add this to lib/index.scm:"
	@echo '  ((ffi libffi) "ffi/libffi.scm")'
	@echo ""

example: all
	@$(MICKEY_LIB_PATH) $(MICKEY)/mickey -L. example.scm

clean:
	rm -f $(TARGETS)
