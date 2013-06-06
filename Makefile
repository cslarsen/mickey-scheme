# See src/Makefile for compilation options

PORTABLE_TESTS = test/hello.scm \
                 test/begin.scm \
                 test/math.scm  \
                 test/strings.scm

all-w/message: all
	@echo ""
	@echo "------------------------------------------"
	@echo "Compilation seems to have been successful!"
	@echo "------------------------------------------"
	@echo ""
	@echo "To run mickey on Linux, be sure to do set the library path, e.g."
	@echo ""
	@echo '  $$ LD_LIBRARY_PATH=".:" ./mickey'
	@echo ""
	@echo "or simply"
	@echo ""
	@echo '  $$ export LD_LIBRARY_PATH=".:"'
	@echo '  $$ ./mickey'
	@echo ""
	@echo "You can now try either of"
	@echo ""
	@echo "  make check      # for quick tests"
	@echo "  make check-all  # for more tests"
	@echo "  make run        # to enter the REPL"
	@echo ""

all:
	@cd src ; make all
	@cp src/mickey src/libmickey.so .
	@cp src/lib*.so lib/

mickey:
	cd src ; make mickey
	cp src/mickey src/libmickey.so .

run: all
	@LD_LIBRARY_PATH=".:" ./mickey

runz: all
	@LD_LIBRARY_PATH=".:" ./mickey -z

runv: all
	@LD_LIBRARY_PATH=".:" ./mickey -v

runvz: all
	@LD_LIBRARY_PATH=".:" ./mickey -v -z

check: all
	LD_LIBRARY_PATH=".:" ./mickey -Itest test/tests.scm

check-all: all
	for t in test/*.scm; do LD_LIBRARY_PATH=".:" ./mickey -Itest $$t; done

check-diff: all
	# mickey and chicken should have same output
	@echo "=== Chicken Scheme ==="
	@csi -bq $(PORTABLE_TESTS)
	@echo ""
	@echo "=== Mickey Scheme ==="
	@./mickey $(PORTABLE_TESTS)

clean:
	rm -f ./mickey ./libmickey.so
	rm -f lib/*.so
	cd src ; make clean
