#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 求幂

;; 递归计算过程
;; 时间复杂度 n
;; 空间复杂度 n
(define expt_v1
  (lambda (b n)
    (if (= n 0)
        1
        (* b (expt_v1 b (- n 1))))))

;; 迭代计算过程
;; 时间复杂度 n
;; 空间复杂度 1
(define expt_v2
  (lambda (b n)
    (define iter
      (lambda (base count)
        (if (= count 0)
            base
            (iter (* base b)
                  (- count 1)))))
    (iter 1 n)))

;; 递归计算过程, 快速求幂
;; 时间复杂度 lg n
;; 空间复杂度 lg n
(define fast-expt
  (lambda (b n)
    (cond
      ((= n 0) 1)
      ((even? n) (expt (fast-expt b (/ n 2)) 2))
      (else (* b (fast-expt b (- n 1)))))))