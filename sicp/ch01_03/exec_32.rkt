#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

;; 定义抽象过程accumulate
;; 从过程sum和product抽象出来

(define accumulate
  (lambda (combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner null-value term (next a) next b)))))

(define accumulate_
  (lambda (combiner null-value term a next b)
    (define iter
      (lambda (accum count)
        (if (> count b)
            accum
            (iter (combiner accum (term count))
                  (next count)))))
    (iter null-value a)))

;; sum 1 ~ 100
(accumulate + 
            0
            (lambda (x) x)
            1
            (lambda (x) (+ x 1))
            100)

(accumulate_ +
             0
             (lambda (x) x)
             1
             (lambda (x) (+ x 1))
             100)

;; 10!
(accumulate *
            1
            (lambda (x) x)
            1
            (lambda (x) (+ x 1))
            10)

(accumulate_ *
             1
             (lambda (x) x)
             1
             (lambda (x) (+ x 1))
             10)