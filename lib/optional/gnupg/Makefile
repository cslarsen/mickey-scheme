TARGETS = gnupg.o libgnupg.so
CXX = /usr/bin/g++
MICKEY = ../../..

GPGME_CFLAGS = $(shell gpgme-config --cflags)
GPGME_LIBS = $(shell gpgme-config --libs)

CXXFLAGS = -W -Wall -I$(MICKEY)/include $(GPGME_CFLAGS)
LDFLAGS = -L$(MICKEY)/lib -lmickey $(GPGME_LIBS)
LIBFLAGS = -shared -fpic

MICKEY_LIB_PATH = DYLD_LIBRARY_PATH=$(MICKEY) LD_LIBRARY_PATH=$(MICKEY)

all: $(TARGETS)

libgnupg.so: gnupg.o

lib%.so: %.o
	$(CXX) $(CXXFLAGS) $(LIBFLAGS) $(LDFLAGS) $< -o $@

check: all
	@$(MICKEY_LIB_PATH) $(MICKEY)/mickey -L. tests.scm

install: check
	@mkdir -p $(MICKEY)/lib/crypto/gnupg
	@cp -f gnupg.scm $(MICKEY)/lib/crypto/gnupg
	@cp -f libgnupg.so $(MICKEY)/lib/
	@echo ""
	@echo "You need to manually add this to lib/index.scm:"
	@echo '  ((crypto gnupg) "crypto/gnupg/gnupg.scm")'
	@echo ""

example: all
	@$(MICKEY_LIB_PATH) $(MICKEY)/mickey -L. example-show-keys.scm

clean:
	rm -f $(TARGETS)
