Mickey R7RS Scheme
==================

Mickey Scheme is an incomplete, slow and buggy implementation of R7RS Scheme
small.

The current project goals are to

* Provide a _correct_ and _complete_ implementation of R7RS-small (WG1)
* Emphasize clarity and simplicity in the implementation
* Be a powerful platform for experimentation and creation of a more advanced
  scheme compiler.

Note that _Mickey_ is just a codeword for the early stages of this project.
The name will change as the project matures.

Current Features
----------------

  * Implements 77% of R7RS-small; see [compliance report](#compliance)
  * Quotation and quasiquotation
  * All non-syntactic `let`-forms
  * Variadic functions
  * Two macro systems: `syntax-rules` (incomplete support) and
                       `define-macro` (unhygienic)
  * Lazy evaluation with memoization in `(scheme lazy)`
  * Full support for the R7RS library system `define-library`
  * Tail call eliminiation
  * The `define-record-type` record system

Some of these are demonstrated in the [example code section](#examples).

SRFIs
-----

The supported SRFIs are

  * `(srfi 0)` Feature-based conditional expansion construct,
    http://srfi.schemers.org/srfi-0/srfi-0.html
  * `(srfi 1)` List Library, http://srfi.schemers.org/srfi-1/srfi-1.html
  * `(srfi 4)` Homogeneous numeric vector datatypes,
    http://srfi.schemers.org/srfi-4/srfi-4.html
  * `(srfi 8)` receive: Binding to multiple values,
    http://srfi.schemers.org/srfi-8/srfi-8.html
  * `(srfi 9)` Defining record types,
    http://srfi.schemers.org/srfi-9/srfi-9.html
    *NOTE:* Currently only supported via `(scheme base)`
  * `(srfi 16)` Syntax for procedures of variable arity,
    http://srfi.schemers.org/srfi-16/srfi-16.html
  * `(srfi 98)` An interface to access environment variables,
    http://srfi.schemers.org/srfi-98/

See the SRFI page at http://srfi.schemers.org and the list of accepted ones
at http://srfi.schemers.org/final-srfis.html

Extensions
----------

Hacking on Mickey Scheme is extremely easy.  Because of this I couldn't
resist adding:

  * First class environments via the library `(mickey environment)`
    ([see examples](#environments))

It's also easy to write wrapper code for using C libraries:

  * Dynamic loading of shared libraries via `(posix dlopen)`
    ([see dlopen example](#c-libraries))

Current Shortcomings
--------------------

  * It's __slow__: The code is completely interpreted, without _any_
    optimizations.  I have plans to change this, but it's currently not a
    priority.

  * It's __incomplete__: Some key Scheme features are still missing, and
    quite some R7RS library functions (currently it supports 195 of 335
    R7RS definitinos).

  * It's __buggy__: There are inherent bugs in the engine as well as
    erronous implementations of library functions.

  * It does not collect __garbage__: Currently, there is no garbage
    collector.  Adding a simple mark-and-sweep GC is trivial, though, so
    I'll get to it once I think it's important enough.

  * It doesn't support __continuations__ (yet):  I think I'll have to
    convert to continuation passing style for a proper implementation of
    this.

  * Other missing features include dynamic-wind, exception handling and so
    on.  These will be added when call/cc is implemented.

## Compliance

The table below shows how many of the definitions in R7RS-small that have
been implemented in Mickey Scheme.

The first number shows the coverage in percent, then number of implemented
definitions, definitions required by R7RS-small, missing definitions and
name of the library.

     68% 161/236  -75 (scheme base)
    100%     1/1    0 (scheme case-lambda)
    100%   22/22    0 (scheme char)
      0%     0/6   -6 (scheme complex)
    100%   24/24    0 (scheme cxr)
    100%     2/2    0 (scheme eval)
     20%    2/10   -8 (scheme file)
    100%   12/12    0 (scheme inexact)
     40%     2/5   -3 (scheme lazy)
    100%     1/1    0 (scheme load)
    100%     5/5    0 (scheme process-context)
    100%     1/1    0 (scheme read)
    100%     1/1    0 (scheme repl)
    100%     3/3    0 (scheme time)
     50%     2/4   -2 (scheme write)
     86% 186/217  -31 (scheme r5rs)
     77% 425/550 -125 <all>

In summary, Mickey implements 425 of 550 definitions in R7RS-small.
125 definitions have not been implemented.

This corresponds to 77% coverage.

Compiling
---------

To install Mickey Scheme, you need a C++ compiler, dlopen et al. and
readline.  If you don't have readline, remove `-DUSE_READLINE` from
`src/Makefile`.

As the project is in its early stages, I haven't used any of the GNU
autotools for building the project (should you feel inclined to create the
configure scripts needed, patches are warmly welcomed!).

The code has been developed on OS X and Linux, so if you have the packages
mentioned above, compilation should go smoothly.

To compile and run, just type

    $ make -j run

to fire up a REPL.  On Linux, you have to set `LD_LIBRARY_PATH` to be able to
locate dynamic shared objects in your current directory:

    $ CXX=c++ make -ej
    ...
    $ LD_LIBRARY_PATH=".:" ./mickey

or, alternatively just export it into your environment:

    $ export LD_LIBRARY_PATH=".:"
    $ ./mickey

The equivalent on Mac OS X is `DYLD_LIBRARY_PATH`.

To run some test code, just type

    $ make check

or

    $ make check-all

That's all there is to it!

Feature flags
-------------

  * `-DUSE_READLINE` and `-lreadline` for readline support (including TAB
     completion)

License
-------

Mickey R7RS Scheme
Copyright (C) 2011-2013 Christian Stigen Larsen

Distributed under version 2.1 of the Lesser GNU Public License (LGPL) while
also allowing anyone to change the license on a particular copy of the code
to the LGPL 3.0, the GPL 2.0 or the GPL 3.0.

See the file COPYING for the full text of the LGPL 2.1.

Author
------

Christian Stigen Larsen <csl@sublevel3.org> http://csl.sublevel3.org

## Examples

Here are a few example code snippets for Mickey Scheme.  

Besides demonstrating the basic capabilities of Mickey, it also serves as a
kind of soft introduction to Scheme.

First, let's start `mickey`.

    $ ./mickey
    #|                                                                 _
       Mickey Scheme (C) 2011-2012 Christian Stigen Larsen              \
       4.2.1 Compatible Apple Clang 4.0 ((tags/Apple/clang-421.0.57))   /\
       Readline 4.2                                                    /  \_

       To quit, hit CTRL+D or type (exit).  Use (help) for an introduction.
       Distributed under the LGPL 2.1; see LICENSE
    |#

    #; mickey> 

If you're on Linux, you may have to set `LD_LIBRARY_PATH` to find the shared
object files:

    $ LD_LIBRARY_PATH=".:" ./mickey

This is the REPL, short for _read-evaluate-print loop_.

First, you should note two things:  The `#| banner inside a comment |#` and
the `#;` symbol.  They make it possible to copy the code you see here and
paste it directly into your own REPL, without needing to remove `mickey>`
and so on.

The symbol `#;` means "ignore the next symbol"; so `#; mickey> (+ 1 2 3)`
means that Mickey Scheme will skip `mickey>` and move on to `(+ 1 2 3)`,
which it then will evaluate.  This is a known trick in scheme systems.

Now, let's type some code.

    #; mickey> (display "Hello, world!\n")
    Hello, world!

## Lambda

Let's play with lambdas.  First we'll create a lambda to square a
number and execute it on the fly.

    #; mickey> ((lambda (x) (* x x)) 12)
    144

We can bind it to a variable as well.  Let's bind it to the variable
`square`.

    #; mickey> (define square (lambda (x) (* x x)))
    #; mickey> (square 12)
    144
    #; mickey> (square 3.1415)
    9.86902

Let's have some fun with lambdas.  Let's create a function that creates
other functions.

We'll create a general function `make-adder` that creates a function that
can add a static number to another number.

    #; mickey> (define make-adder
                 (lambda (frozen-number)
                   (lambda (x)
                     (+ x frozen-number))))

As you can see, we give it a `frozen-number` and it returns a lambda that
takes a number `x` and adds the two together.

So to make a function that adds 5 to its argument, we simply do

    #; mickey> (define add5 (make-adder 5))
    #; mickey> (add5 10)
    15

Likewise

    #; mickey> (define add17 (make-adder 17))
    #; mickey> (add17 10)
    27

Writing `(define (lambda (x) ...))` is tedious, so a shorter variant is
available:

    #; mickey> (define (cube x) (* x x x))
    #; mickey> (cube 101)
    1030301

Mickey uses GNU readline, so it offers both tab completion and history.  And
it actually works, too.  Here we write `(ca` and hit `TAB` two times to see
available definitions.

    #; mickey> (ca
    caaar  caadr  caar   cadr   car  

Here are `car` and `cdr`.  They extract the first and remaining items in a
list.

    #; mickey> (car '(1 2 3))
    1
    #; mickey> (cdr '(1 2 3)))
    (2 3)

I like the old school "car" and "could-er" forms because you can compose
them, so that you can extract the `car` of the `cdr` like so:

    #; mickey> (cadr '(1 2 3))
    2

They might not be very interesting for flat lists, but they shine for
accessing trees.

## Arithmetic

Here is some simple arithmetic.

    #; mickey> (+ 1 2 3 4 5 6 7 8 9 10)
    55

Summation of sequences can be calculated more quickly with

    #; mickey> (define (seq-sum n)
              (* n (/ (+ n 1) 2)))

which gives us

    #; mickey> (seq-sum 10)
    55
    #; mickey> (seq-sum 100)
    5050
    #; mickey> (seq-sum 127)
    8128

## Let-forms

Here is an example of the "let star" form.  It creates a local variable
scope, and evaluates them in the given order.

    #; mickey> (let* ((x 2)
                      (y 3)
                      (z (* x y)))
                     (display z))

This, of course, prints `6`.

There is also a `letrec` form that allows for mutually recursive
definitions.  Or in plain english, expressions that refer to each other.

The typical example of this is to implement `even?` and `odd?` in terms of
each other:

* A number is _even_ if the preceding number is odd, and
* A number is _odd_ if the preceding number is even

Since our definition is going to be mutually recursive, we need to handle
base cases of zero (negative values will make the code loop forever,
though).

Let's write that out, along with a `check-number` function.

    (letrec
      ((even? (lambda (n)
                (if (zero? n) #t
                    (odd? (- n 1)))))

       (odd? (lambda (n)
               (if (zero? n) #f
                   (even? (- n 1)))))

       (check-number
         (lambda (n)
           (display `(The number ,n is ,(if (even? n) 'even 'odd)))
           (newline))))

       (check-number 2)
       (check-number 3)
       (check-number 88)
       (check-number 99))

If you run the above code in `mickey`, you'll get this output:

    (The number 2 is even)
    (The number 3 is odd)
    (The number 88 is even)
    (The number 99 is odd)

## Macros

Now, Scheme doesn't have a `when` function.  The `when` function checks
whether the first argument is true.  If it is, then it will evaluate --- or
execute --- the code given in the remaining arguments.

You can't do this with a simple function, because it would *always* evaluate
the code body.  As an example, let's say we have a boolean variable
`green-light`.  If it's `true`, we'll format the hard drive:

    (when green-light (format-drive))

If `when` was implemented as a function, it would always format the hard
drive, because function parameters must be evaluated _before_ entering the
function itself.

It is clear that we need a way to _control evaluation_.  Scheme's macros
will let ut do exactly that.

So let's implement `when` as a hygienic macro.

    #; mickey> (define-syntax when
                 (syntax-rules ()
                   ((when test expr ...)
                     (if test (begin expr ...)))))

That's it.  To demonstrate that we control the evaluation, let's create a
function with a side effect that prints to the console when it's evaluated.

    #; mickey> (define (say-hello) (display "Hello\n"))
    #; mickey> (say-hello)
    Hello

Calling

    #; mickey> (when #f (say-hello))

does not print anything, which is good.  In contrast,

    #; mickey> (when #t (say-hello))
    Hello

does indeed print to the console.

## Quotation

Now let's try some examples with quasi-quotation.  Since Scheme is a
symbolic language, we can easily create syntax trees for languages like SQL
by just using quotation.  But sometimes we'll want to embed actual
computations into them, so therefore we can use the specual `unquote` prefix
with a comma.

Here is an example of just that.

    #; mickey> (define (sql-get-user name)
                 `(select * from user where name = ,name))

*Note*: _There is currently a bug in the mickey REPL, so that
quasiquotation requires an extra closing parenthesis to parse.  
This bug is not present if you run this example from a file, though._

Running the function should be self-explanatory.

    #; mickey> (sql-get-user "foo")
    (select * from user where name = "foo")

Furthermore, sometimes we want to splice two lists together when we quote.
We can do that by using unquote splice, or the `,@` prefix.

    #; mickey> (define date '(2012 5 17))
    #; mickey> date
    (2012 5 17)
    #; mickey> `(here is a date: ,@date)
    (here is a date: 2012 5 17)

## Lazy evaluation

Mickey Scheme also supports delayed -- or lazy -- evaluation.  That is,
computations that are not executed right away.

In fact, all languages that support evaluation control (for instance, via a
macro facility) and first class closures should be able to *implement* lazy
evaluation without any external library.

Let's create a list `queue` that contains some code we want to execute at a
later time.

    (define queue
      (list (delay (display "One! "))
            (delay (display "Three! "))
            (delay (display "Two! "))))

Now we want to execute the code in the list, but reordered so that it will
print "One! Two! Three!":

    (force (list-ref queue 0))
    (force (list-ref queue 2))
    (force (list-ref queue 1))

This outputs:

    One! Two! Three!

## Environments

Mickey Scheme has an experimental library for first-class environments.
It bascially implements the API from MIT Scheme, with a few missing
features.

Here is an example on how to use it.

First, let's start up mickey with an empty environment (the -z argument).
An empty environment means that the REPL only has one bound definition, that
of `import`.

    $ ./mickey -z
    #|                                                                 _
       Mickey Scheme (C) 2011-2012 Christian Stigen Larsen              \
       4.2.1 (Based on Apple Inc. build 5658) (LLVM build 2336.11.00)   /\
       Readline 4.2                                                    /  \_

       To quit, hit CTRL+D or type (exit).  Use (help) for an
       introduction.
    |#

Now, let's just make sure that we don't have any binding for `foo`.

    #; mickey> foo
    Unbound definition: foo

Let's import the `(mickey environment)` library and define `foo` in a new
environment that is a child of the current one.

    #; mickey> (import (mickey environment))
    #; mickey> (define sub-environment
                         (make-environment
                           (define foo 123)))

We still should not see `foo` in the current environment:

    #; mickey> foo
    Unbound definition: foo

But it should be available in the environment stored in `sub-environment`.

    #; mickey> (environment-bindings sub-environment)
    ((foo 123))

Let's import `(scheme write)` so we can call `display` and the `newline`
function from `(scheme base)`.

    #; mickey> (import (scheme write))
    #; mickey> (import (only (scheme base) newline))

Now, let's evaluate an expression in the child environment to print the
value of `foo`:

    #; mickey> (environment-eval 
                 (begin
                   (display foo)
                   (newline)) sub-environment)
    123
    #; mickey> ^D

So, what's the point with first class environments?  You can use it do to
stuff like implementing your own module system.  The reason they are not
part of standard Scheme is because they make it very difficult to reason
about what a program does just by reading the code.

Therefore, implementations are allowed to implement them on their own.

## C-libraries

It's possible to write libraries in C and call them from Mickey Scheme.

Since Mickey does not use a build system yet, the process is a little bit
cumbersome.

First you write a small library in C.  Let's say we want to be able to call
the `uname(3)` function from Mickey.  We'll just write some wrapper code in
C:

    #include <sys/utsname.h>
    #include "mickey.h"

    extern "C" cons_t* proc_uname(cons_t* args, environment_t* env)
    {
      struct utsname p;

      if ( uname(&p) != 0 )
        return boolean(false);

      // Return utsname as an associative list
      return
        cons(cons(symbol("sysname",  NULL), cons(string(p.sysname))),
        cons(cons(symbol("nodename", NULL), cons(string(p.nodename))),
        cons(cons(symbol("release",  NULL), cons(string(p.release))),
        cons(cons(symbol("version",  NULL), cons(string(p.version))),
        cons(cons(symbol("machine",  NULL), cons(string(p.machine))))))));
    }

Note that if you use a C++ compiler with the above code, you must prefix the
function with `extern "C"` to avoid the infamous C++ name-mangling.

To compile this, do something à la

    gcc -shared -fPIC -I<mickey path> mickey-uname.c \
        -L<mickey path> -lmickey -o libmickey-uname.so

This should give you a libmickey-uname.so file.  To load this file from
Mickey, we have to start mickey and then import the `(posix dlopen)` library.

    csl$ ./mickey
    #|                                                                 _
       Mickey Scheme (C) 2011-2012 Christian Stigen Larsen              \
       4.2.1 (Based on Apple Inc. build 5658) (LLVM build 2336.11.00)   /\
       Readline 4.2                                                    /  \_

       To quit, hit CTRL+D or type (exit).  Use (help) for an
       introduction.
    |#

    #; mickey> (import (posix dlopen))

Let's load the library using the dlopen options `RTLD_NOW` and `RTLD_LOCAL`.
You can omit the options to use default dlopen mode.  I'm just showing you
how to specify several options.

    #; mickey> (define lib-uname (dlopen "libmickey-uname.so" 'now 'local))

Now `lib-uname` contains a handle to the library.

    #; mickey> lib-uname
    #<pointer 'dynamic-shared-library-handle' 0x7ffb63436460>

Let's get a reference to the `proc_uname` function and bind that to the
variable `uname`:

    #; mickey> (define uname (dlsym lib-uname "proc_uname"))

If `dlsym` fails, it will return `#f`.  Let's see if it worked:

    #; mickey> uname
    #<closure 0x7ffb6343b550>

It did!  Let's try executing it.

    #; mickey> (uname)
     ((sysname "Darwin") (nodename "Christians-mac-8.local") (release "12.0.0")
     (version "Darwin Kernel Version 12.0.0: Sun Jun 24 23:00:16 PDT 2012;
     root:xnu-2050.7.9~1/RELEASE_X86_64") (machine "x86_64"))

It returns the struct as an a-list, so we can do

    #; mickey> (assv 'version (uname))
    (version "Darwin Kernel Version 12.0.0: Sun Jun 24 23:00:16 PDT 2012;
    root:xnu-2050.7.9~1/RELEASE_X86_64")

and

    #; mickey> (assv 'machine (uname))
    (machine "x86_64")

Easy!

## Custom Scheme-libraries in R7RS

To create a small library, use the `define-library` form as described in the
R7RS draft over at http://scheme-reports.org

As an example, put this in a file `foo/bar.scm`:

    (define-library (foo bar)
      (import (only (scheme base) car cdr define))
      (export first last head tail)
      (begin
        (define (first x) (car x))
        (define (last x) (cdr x))
        (define (head x) (car x))
        (define (tail x) (cdr x))
        (define (rest x) (cdr x))))

Mickey needs to know where to find this library, so you need to add an entry
in `lib/index.scm`:

  ; ...
  ((foo bar) "foo/bar.scm")
  ; ...

There really isn't more to it.  You can take a look in the `lib/` directory
for some ideas.  Note that you _have_ to import primitive functions to be
able to use them, even core forms such as `define`.  Also note that even
though we define `rest` above, it is not exported, and thus not available
_outside_ of the library.

The library system _mostly_ complete, but lacks some intricate features that
can be found in R7RS.  One thing is that I don't think Mickey gracefully
handles circular dependencies.

## (define-library ...)

Note that in R7RS, you cannot simply open up a REPL and start writing your
own library using `(define-library (foo schmoo) ...)`.  That means, the
standard doesn't say much about it, so it is most likely up to implementors.

I think this may turn out to be surprising to many newcomers to R7RS, so I
will probably allow it in Mickey.
