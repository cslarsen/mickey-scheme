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
	cd $@; make; cd ..

libffi: .DUMMY
	cd $@; make; cd ..

all: $(TARGETS)

check: all

clean:
	for dir in $(TARGETS); do \
		cd $$dir && make clean; \
		cd ..; \
	done

install:
	@true
