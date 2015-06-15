#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

;; 定义类似sum的product过程, 需要定义一个迭代计算过程和一个递归计算过程

(define product
  (lambda (term a next b)
    (if (> a b)
        1
        (* (term a) (product term (next a) next b)))))

(define product_
  (lambda (term a next b)
    (define iter
      (lambda (null-value counter)
        (if (> counter b)
            null-value
            (iter (* null-value (term counter))
                  (next counter)))))
    (iter 1 a)))

;; 计算pi/4的近似

(define pi-product
  (lambda (n)
    (product (lambda (x)
               (if (even? x)
                   (/ (+ x 2) (+ x 1))
                   (/ (+ x 1) (+ x 2))))
             1
             (lambda (x) (+ x 1))
             n)))

(define pi-product_
  (lambda (n)
    (product_ (lambda (x)
                (if (even? x)
                    (/ (+ x 2) (+ x 1))
                    (/ (+ x 1) (+ x 2))))
              1
              (lambda (x) (+ x 1))
              n)))
                
(* 4.0 (pi-product 1000))
(* 4.0 (pi-product_ 1000))
                       