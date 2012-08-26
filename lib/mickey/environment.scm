(define-library (mickey environment)
  #|

  Copyright (C) 2012 Christian Stigen Larsen
  http://csl.sublevel3.org

  Distributed under the LGPL 2.1; see LICENSE

  DESCRIPTION

  This module presents a first class environment to the user.  It is
  directly modelled on the API provided by MIT Scheme, and is nearly
  source-level compatible with it.

  http://sicp.ai.mit.edu/Fall-2004/manuals/scheme-7.5.5/doc/scheme_14.html

  Currently unimplemented:

    * The environment variables system-global-environment
                                user-initial-environment

    * The procedures (nearest-repl/environment)
                     (ge environment)
                     (interpreter-environment? obj)


  Also note that the eval procedure is exported as environment-eval.

  USAGE EXAMPLE

    (import (mickey environment))
    (define env (make-environment))
    (environment-eval (begin (define a 123) env)
    ; defines a=123 in the environment env

  |#

  (export
    bound?
    environment-assign!
    environment-assignable?
    environment-bindings
    environment-bound-names
    environment-bound?
    environment-eval
    environment-has-parent?
    environment-lookup
    environment-parent
    environment?
    make-environment
    the-environment)
  (import (only (scheme base) define)
          (mickey library))
  (begin

    (open-internal-library "libmickey-environment.so" 'lazy)

    (define bound?                  (bind-procedure "proc_boundp"))
    (define environment-assign!     (bind-procedure "proc_env_assign"))
    (define environment-assignable? (bind-procedure "proc_env_assignablep"))
    (define environment-bindings    (bind-procedure "proc_env_bindings"))
    (define environment-bound-names (bind-procedure "proc_env_bound_names"))
    (define environment-bound?      (bind-procedure "proc_env_boundp"))
    (define environment-eval        (bind-syntax    "proc_env_eval"))
    (define environment-has-parent? (bind-procedure "proc_env_has_parentp"))
    (define environment-lookup (bind-procedure "proc_env_lookup"))
    (define environment-parent (bind-procedure "proc_env_parent"))
    (define environment?       (bind-procedure "proc_envp"))
    (define make-environment   (bind-syntax    "proc_make_environment"))
    (define the-environment    (bind-procedure "proc_the_environment"))))
