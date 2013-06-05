TARGETS = ffi.o libffi.so
CXX = /usr/bin/g++
MICKEY = ../../..

##
## LOCATION OF LIBFFI
## You should use a recent version of libffi that
## contains the ffi_prep_cif_var() function.
##
## LIBFFI_CFLAGS = -I/usr/include/ffi
##
## I installed libcurl using homebrew on OS X, which resulted
## in the following directories:
##
LIBFFI_CFLAGS = -I/usr/local/opt/libffi/lib/libffi-3.0.13/include/
LIBFFI_LIBS = -L/usr/local/opt/libffi/lib/ -lffi

CXXFLAGS = -W -Wall -I$(MICKEY)/include $(LIBFFI_CFLAGS)
LDFLAGS = $(LIBFFI_LIBS) -L$(MICKEY)/lib -lmickey
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

repl: all
	@$(MICKEY_LIB_PATH) $(MICKEY)/mickey -L.

example: all
	@$(MICKEY_LIB_PATH) $(MICKEY)/mickey -L. example.scm

clean:
	rm -f $(TARGETS)
