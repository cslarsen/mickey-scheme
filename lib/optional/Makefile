TARGETS = gnupg libffi

help:
	@echo "This directory contains optional modules you can build"
	@echo "for Mickey Scheme."
	@echo ""
	@echo "They are optional because they depend on C libraries that may not"
	@echo "be available on all systems."
	@echo ""
	@echo "Available build targets:"
	@echo ""
	@echo "  all check install clean"
	@echo "  $(TARGETS)"
	@echo ""

.DUMMY:
	@true

gnupg: .DUMMY
libffi: .DUMMY

all: $(TARGETS)

%:
	cd $@ && make

check: all

clean:
	@true

install:
	@true
