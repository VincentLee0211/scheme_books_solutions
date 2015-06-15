#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 阶乘的计算过程"形状"

;; 递归计算过程
(define factorial_v1
  (lambda (n)
    (if (= n 1)
        1
        (* n (factorial_v1 (- n 1))))))

(factorial_v1 6)
(* 6 (factorial_v1 5))
(* 6 (* 5 (factorial_v1 4)))
(* 6 (* 5 (* 4 (factorial_v1 3))))
(* 6 (* 5 (* 4 (* 3 (factorial_v1 2)))))
(* 6 (* 5 (* 4 (* 3 (* 2 (factorial_v1 1))))))

(* 6 (* 5 (* 4 (* 3 (* 2 1)))))
(* 6 (* 5 (* 4 (* 3 2))))
(* 6 (* 5 (* 4 6)))
(* 6 (* 5 24))
(* 6 120)
720
   

;; 迭代计算过程
(define factorial_v2
  (lambda (n)
    (factorial-iter 1 1 n)))

(define factorial-iter
  (lambda (producter counter max-count)
    (if (> counter max-count)
        producter
        (factorial-iter (* producter counter)
                       (+ counter 1)
                       max-count))))

(factorial_v2 6)
(factorial-iter 1 1 6)
(factorial-iter 1 2 6)
(factorial-iter 2 3 6)
(factorial-iter 6 4 6)
(factorial-iter 24 5 6)
(factorial-iter 120 6 6)
(factorial-iter 720 7 6)
720

;; 迭代过程的标准写法
(define factorial_v3
  (lambda (n)
    (define iter
      (lambda (product count)
        (if (> count n)
            product
            (iter (* product count)
                  (+ count 1)))))
    (iter 1 1)))