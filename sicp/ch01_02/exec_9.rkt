#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 辅助函数
;; (define zero? (lambda (x) (= x 0)))
(define add1 (lambda (x) (+ x 1)))
(define sub1 (lambda (x) (- x 1)))

;; 用代换模型展开下面2个计算过程

;; 递归计算过程
(define +_v1
  (lambda (a b)
    (if (zero? a)
        b
        (add1 (+_v1 (sub1 a) b)))))

(+_v1 4 5)
(add1 (+_v1 3 5))
(add1 (add1 (+_v1 2 5)))
(add1 (add1 (add1 (+_v1 1 5))))
(add1 (add1 (add1 (add1 (+_v1 0 5)))))

(add1 (add1 (add1 (add1 5))))
(add1 (add1 (add1 6)))
(add1 (add1 7))
(add1 8)
9

;; 迭代计算过程
(define +_v2
  (lambda (a b)
    (if (zero? a)
        b
        (+_v2 (sub1 a) (add1 b)))))

(+_v2 4 5)
(+_v2 3 6)
(+_v2 2 7)
(+_v2 1 8)
(+_v2 0 9)
9