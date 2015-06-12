#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

(#%require (only racket/base current-inexact-milliseconds))

(define fast-expt
  (lambda (b n)
    (cond
      ((= n 0) 1)
      ((even? n) (expt (fast-expt b (/ n 2)) 2))
      (else (* b (fast-expt b (- n 1)))))))

(define expmod_
  (lambda (base exp m)
    (remainder (fast-expt base exp) m)))

;; 需要面对大整数的取模运算
;; 计算速度比预期的要慢的多

(define expmod
  (lambda (base exp m)
    (cond
      ((= exp 0) 1)
      ((even? exp)
       (remainder (expt (expmod base (/ exp 2) m) 2) m))
      (else
       (remainder (* base (expmod base (- exp 1) m)) m)))))

(define test-times
  (lambda (f)
    (lambda (base exp m)
      (let ((now (current-inexact-milliseconds)))
        (f base exp m)
        (- (current-inexact-milliseconds) now)))))

;; test
;; 0.1秒内出结果
((test-times expmod) 285 123456789123 123456789123)

;; 无法停机, 要等很久哈
((test-times expmod_) 285 123456789123 123456789123)