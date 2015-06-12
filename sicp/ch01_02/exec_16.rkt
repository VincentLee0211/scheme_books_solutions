#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 迭代计算过程, 快速求幂
(define fast-expt
  (lambda (b n)
    (define iter
      (lambda (a b n)
        (cond
          ((= n 0) a)
          ((even? n) (iter a (expt b 2) (/ n 2)))
          (else (iter (* a b) b (- n 1))))))
    (iter 1 b n)))