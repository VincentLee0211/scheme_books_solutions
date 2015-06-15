#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; R5RS 不存在random过程
(#%require (only racket/base random))

;; 素数检测

;; 时间复杂度n ^ .5的素数算法

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

;; 概率算法

;; 费马小定理
;; 若n是一个素数, a是小于n的任意正整数, 
;; 那么a的n次方与a模n(其实就是a)同余

;; 过程expmod, 计算一个数的幂对另一个数取模的结果

;; 对任意的x, y, m, 可以通过分别计算x取模m和y取模m, 然后将它们相乘之后取模m, 等于x乘y取模m
(define expmod
  (lambda (base exp m)
    (cond
      ((= exp 0) 1)
      ((even? exp)
       (remainder (expt (expmod base (/ exp 2) m) 2) m))
      (else
       (remainder (* base (expmod base (- exp 1) m)) m))))) ;; (remainder base m) = base

(define fast-prime?
  (lambda (n times)
    (cond
      ((= times 0) #t)
      ((fermat-test n) (fast-prime? n (- times 1)))
      (else #f))))

(define fermat-test
  (lambda (n)
    (define try
      (lambda (test)
        (= (expmod test n n) test)))
    (try (+ 1 (random (- n 1))))))