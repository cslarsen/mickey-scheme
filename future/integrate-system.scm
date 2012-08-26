#|
(The following code and text has been taken from R7RS).

Integrate-system integrates the system

 $y´_k = f_k(y_1,y_2,\ldots,y_n), k=1,\ldots,n$

of differential equations with the method of Runge-Kutta.

The parameter system-derivative is a function that takes a system state (a
vector of values for the status variables y_1, ..., y_n) and produces a
system derivative (the values y'_1,...,y'_n).

The parameter initial-state provides an initial system state, and h is an
initial guess for the length of the integration step.

The value returned by integrate-system is an infinite stream of system
states.
|#
(define integrate-system
  (lambda (system-derivative initial-state h)
    (let ((next (runge-kutta-4 system-derivative h)))
      (letrec ((states
                (cons initial-state
                      (delay (map-streams next
                                          states)))))
       states))))

#|
Runge-kutta-4 takes a function, f, that produces a system derivative from a
sytem state.  Runge-kutta-4 produces a function that takes a system state
and produces a new system state.
|#
(define runge-kutta-4
  (lambda (f h)
    (let ((*h (scale-vector h))
          (*2 (scale-vector 2))
          (*1/2 (scale-vector (/ 1 2)))
          (*1/6 (scale-vector (/ 1 6))))
      (lambda (y)
        ;; y is a system state
        (let* ((k0 (*h (f y)))
               (k1 (*h (f (add-vectors y (*1/2 k0)))))
               (k2 (*h (f (add-vectors y (*1/2 k1)))))
               (k3 (*h (f (add-vectors y k2)))))
          (add-vectors y
            (*1/6 (add-vectors k0
                               (*2 k1)
                               (*2 k2)
                               k3))))))))

(define elementwise
  (lambda (f)
    (lambda vectors
      (generate-vector
        (vector-length (car vectors))
        (lambda (i)
          (apply f
                 (map (lambda (v) (vector-ref v i))
                      vectors)))))))

(define generate-vector
  (lambda (size proc)
    (let ((ans (make-vector size)))
      (letrec ((loop
                (lambda (i)
                  (cond ((= i size) ans)
                        (else
                          (vector-set! ans i (proc i))
                          (loop (+ i 1)))))))
        (loop 0)))))

(define add-vectors (elementwise +))

(define scale-vector
  (lambda (s)
    (elementwise (lambda (x) (* x s)))))

#|
Map-streams is analogous to map: it applies its first argument (a procedure)
to all the elements of its second argument (a stream).
|#
(define map-streams
  (lambda (f s)
    (cons (f (head s))
          (delay (map-streams f (tail s))))))

#|
Infinite streams are implemented as pairs whose car holds the first element
of the stream and whose cdr holds a promise to deliver the rest of the
stream.
|#
(define head car)
(define tail  
  (lambda (stream) (force (cdr stream))))

#|
The following illustrates the use of integrate-system in integrating the
system

  $ C \frac{dv_C}{dt} = -i_L - \frac{v_C}{R} $
  $ L \frac{di_L}{dt} = v_C $

which models a damped oscillator.
|#
(define damped-oscillator
  (lambda (R L C)
    (lambda (state)
      (let ((Vc (vector-ref state 0))
            (Il (vector-ref state 1)))
        (vector (- 0 (+ (/ Vc (* R C)) (/ Il C)))
                (/ Vc L))))))

(define the-states
  (integrate-system
    (damped-oscillator 10000 1000 .001)
    '#(1 0)
    .01))

(display "the-states => ")
(display the-states)
(newline)
