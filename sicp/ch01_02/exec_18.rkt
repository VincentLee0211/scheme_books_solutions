#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 设计一个快速求乘法的迭代过程

(define double (lambda (x) (+ x x)))

(define halve (lambda (x) (/ x 2)))

(define fast-*
  (lambda (a b)
    (define iter 
      (lambda (x y z)
        (cond
          ((= z 0) x)
          ((even? z) (iter x (double y) (halve z)))
          (else (iter (+ x y) y (- z 1))))))
    (iter 0 a b)))
  