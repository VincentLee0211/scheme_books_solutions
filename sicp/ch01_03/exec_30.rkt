#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

;; 将递归计算过程的sum, 改写为迭代计算过程的sum
(define sum
  (lambda (term a next b)
    (define iter
      (lambda (null-value count)
        (if (> count b)
            null-value
            (iter (+ null-value (term count))
                  (next count)))))
    (iter 0 a)))