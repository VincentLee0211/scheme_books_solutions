#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

(define f
  (lambda (g)
    (g 2)))

(define square
  (lambda (x)
    (expt x 2)))

(f square) ;; 4

(f (lambda (z) (* z (+ z 1)))) ;; 6

(f f) ;; 错误, 带入之后获取(2 2), 而(2 ...)不是一个合法的过程