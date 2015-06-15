#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

;; 辅助函数
(define prime?
  (lambda (n)
    (define smallest-divisor
      (lambda (n)
        (find-divisor n 2)))
    (define find-divisor
      (lambda (n test)
        (cond
          ((> (expt test 2) n) n)
          ((divides? n test) test)
          (else (find-divisor n (next test))))))
    (define divides?
      (lambda (n test)
        (= (remainder n test) 0)))
    (define next
      (lambda (test)
        (if (= test 2)
            (+ test 1)
            (+ test 2))))
    (= (smallest-divisor n) n)))

(define gcd_
  (lambda (n d)
    (if (= d 0)
        n
        (gcd_ d (remainder n d)))))

;; 给accumulate添加过滤器

;; 过滤的规则
;; 只组合起由给定范围得到的项里的那些满足特定条件的项

;; 递归计算过程
(define filter-accumulate
  (lambda (filter combiner null-value term a next b)
    (cond
      ((> a b) null-value)
      ((filter a)
       (combiner (term a)
                 (filter-accumulate filter 
                                    combiner 
                                    null-value 
                                    term 
                                    (next a) 
                                    next 
                                    b)))
      (else
       #|
       (combiner null-value
                 (filter-accumulate filter 
                                    combiner 
                                    null-value 
                                    term 
                                    (next a) 
                                    next 
                                    b)))
       |#
       (filter-accumulate filter 
                          combiner 
                          null-value 
                          term 
                          (next a) 
                          next 
                          b)))))

;; 迭代计算过程
(define filter-accumulate_
  (lambda (filter combiner null-value term a next b)
    (define iter
      (lambda (accum count)
        (cond
          ((> count b) accum)
          ((filter count) 
           (iter (combiner accum
                           (term count))
                 (next count)))
          (else
           #|
           (iter (combiner accum
                           null-value)
                 (next count))))))
           |#
           (iter accum (next count))))))
    (iter null-value a)))

;; 求区间[a,b]的素数和
(define sum-prime
  (lambda (a b)
    (filter-accumulate prime?
                       +
                       0
                       (lambda (x) x)
                       a
                       (lambda (x) (+ x 1))
                       b)))

(sum-prime 2 20)

(define sum-prime_
  (lambda (a b)
    (filter-accumulate_ prime?
                        +
                        0
                        (lambda (x) x)
                        a
                        (lambda (x) (+ x 1))
                        b)))

(sum-prime_ 2 20)

;; 小于n的, 与n互素的正整数乘积
(define sum-gcd
  (lambda (n)
    (filter-accumulate (lambda (x) (= (gcd x n) 1))
                       *
                       1
                       (lambda (x) x)
                       1
                       (lambda (x) (+ x 1))
                       n)))

(sum-gcd 20)

(define sum-gcd_
  (lambda (n)
    (filter-accumulate_ (lambda (x) (= (gcd x n) 1))
                        *
                        1
                        (lambda (x) x)
                        1
                        (lambda (x) (+ x 1))
                        n)))

(sum-gcd_ 20)