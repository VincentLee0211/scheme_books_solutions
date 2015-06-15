#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

;; 实现复合函数

(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define add1 (lambda (x) (+ x 1)))

(define square (lambda (x) (* x x)))

((compose square add1) 6)

((compose add1 square) 6)