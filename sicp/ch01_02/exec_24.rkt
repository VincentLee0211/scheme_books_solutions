#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

(#%require (only racket/base current-inexact-milliseconds))
(#%require (only racket/base random))

(define fast-prime?
  (lambda (n times)
    (cond
      ((= times 0) #t)
      ((fermat-test n) (fast-prime? n (- times 1)))
      (else #f))))

(define fermat-test
  (lambda (n)
    (define try
      (lambda (r)
        (= (expmod r n n) r)))
    (try (+ 1 (random (- n 1))))))

(define expmod
  (lambda (base exp m)
    (cond
      ((= exp 0) 1)
      ((even? exp)
       (remainder (expt (expmod base (/ exp 2) m) 2) m))
      (else
       (remainder (* base (expmod base (- exp 1) m)) m)))))

(define timed-prime-test
  (lambda (n times)
    (newline)
    (display n)
    (start-prime-test n times (current-inexact-milliseconds))))

(define start-prime-test
  (lambda (n times start-time)
    (if (fast-prime? n times)
        (report-prime (- (current-inexact-milliseconds) start-time)))))

(define report-prime
  (lambda (elapsed-time)
    (display " *** ")
    (display elapsed-time)))

(define search-for-primes
  (lambda (low high times)
    (let ((low-odd (if (odd? low) low (+ low 1)))
          (high-odd (if (odd? high) high (- high 1))))
      (define iter
        (lambda (start)
          (if (> start high-odd)
              (begin (newline) 'done)
              (begin (timed-prime-test start times)
                     (iter (+ start 2))))))
      (iter low-odd))))

(search-for-primes 1001 1019 1000)

(search-for-primes 10001 10037 1000)

(search-for-primes 100001 100043 1000)

(search-for-primes 1000001 1000039 10000)