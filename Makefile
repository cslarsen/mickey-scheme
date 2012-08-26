# See src/Makefile for compilation options

PORTABLE_TESTS = test/hello.scm \
                 test/begin.scm \
                 test/math.scm  \
                 test/strings.scm

all:
	cd src ; make all
	cp src/mickey src/libmickey.so .
	cp src/lib*.so lib/

mickey:
	cd src ; make mickey
	cp src/mickey src/libmickey.so .

run: all
	./mickey

runz: all
	./mickey -z

check: all
	./mickey -Itest test/tests.scm

check-all: all
	./mickey -Itest test/*.scm

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
