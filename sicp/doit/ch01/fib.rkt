#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 树形递归

;; 递归计算过程
(define fib_v1
  (lambda (n)
    (cond
      ((= n 0) 0)
      ((= n 1) 1)
      (else (+ (fib_v1 (- n 1))
               (fib_v1 (- n 2)))))))

;; 迭代计算过程
(define fib_v2
  (lambda (n)
    (define iter
      (lambda (a b count)
        (if (= count 0)
            b
            (iter (+ a b) a (- count 1)))))
    (iter 1 0 n)))