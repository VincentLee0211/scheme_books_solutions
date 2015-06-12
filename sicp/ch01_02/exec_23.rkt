#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

(#%require (only racket/base current-inexact-milliseconds))

(define prime?
  (lambda (n)
    (define smallest-divisor
      (lambda (n)
        (find-divisor n 2)))
    (define find-divisor
      (lambda (n test)
        (cond
          ((> (expt test 2) n) n)
          ((divides? n test) test)
          (else (find-divisor n (next test))))))
    (define divides?
      (lambda (n test)
        (= (remainder n test) 0)))
    (define next
      (lambda (test)
        (if (= test 2)
            (+ test 1)
            (+ test 2))))
    (= (smallest-divisor n) n)))

(define search-for-primes
  (lambda (low high)
    (let ((low-odd (if (odd? low) low (+ low 1)))
          (high-odd (if (odd? high) high (- high 1))))
      (define iter
        (lambda (start)
          (if (> start high-odd)
              (begin (newline) 'done)
              (begin (times-prime-test start)
                     (iter (+ start 2))))))
      (iter low-odd))))

(define times-prime-test
  (lambda (n)
    (newline)
    (display n)
    (start-prime-test n (current-inexact-milliseconds))))

(define start-prime-test
  (lambda (n start-time)
    (if (prime? n)
        (report-prime (- (current-inexact-milliseconds) start-time)))))

(define report-prime
  (lambda (elapsed-time)
    (display " *** ")
    (display elapsed-time)))

(search-for-primes 1001 1019)

(search-for-primes 10001 10037)

(search-for-primes 100001 100043)

(search-for-primes 1000001 1000039)