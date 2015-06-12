#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; R5RS 不存在random过程
(#%require (only racket/base current-inexact-milliseconds))

;; 计算运行时间

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
          (else (find-divisor n (+ test 1))))))
    (define divides?
      (lambda (n test)
        (= (remainder n test) 0)))
    (define next
      (lambda (test)
        (if (= test 2)
            (+ test 1)
            (+ test 2))))
    (= (smallest-divisor n) n)))

;; 给定范围连续奇数的素性
(define search-for-primes
  (lambda (low high)
    (let ((low-odd (if (odd? low) low (+ low 1)))
          (high-odd (if (odd? high) high (- high 1))))
      (define iter
        (lambda (start)
          (if (> start high-odd)
              (begin (newline) 'done)
              (begin (times-prime-test start)
                     (search-for-primes (+ low 2) high)))))
      (iter low-odd))))
        
(search-for-primes 1001 1019)

(search-for-primes 10001 10037)

(search-for-primes 100001 100043)

(search-for-primes 1000001 1000039)