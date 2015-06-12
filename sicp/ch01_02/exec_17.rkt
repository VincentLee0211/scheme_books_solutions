#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 设计一个快速求乘积的过程

(define double (lambda (x) (+ x x)))

(define halve (lambda (x) (/ x 2)))

(define fast-*
  (lambda (a b)
    (cond
      ((= b 0) 0)
      ((even? b) (double (fast-* a (halve b))))
      (else (+ a (fast-* a (- b 1)))))))