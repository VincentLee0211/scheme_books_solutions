#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 求两个数的最大公约数

(define gcd_ 
  (lambda (n d)
    (if (= d 0)
        n
        (gcd_ d (remainder n d)))))

#|
;; 我又没见过正则序求值的编辑器, 我怎么知道会是这样做呢?
;; 只是因为展开到最后才最终求值这一原则吗?
;; 难道没有记忆体的存在吗? 谁知道呢

;; 正则序
(gcd_ 206 40)

(gcd_ 40 (remainder 206 40))

(if (= t1 0) ...)            ;; 求值(remainder ...)

(gcd t1 (remainder 40 t1))

(if (= t2 0) ...)

(gcd t2 (remainder t1 t2))

(if (= t3 0) ...)

(gcd t3 (remainder t2 t3))

t3

;; t1 = (remainder 206 40)
;; t2 = (remainder 40 t1) = (remainder 40 (remainder 206 40))
;; t3 = (remainder t1 t2) = (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))

;; 1
;; t1 = 1 * 3
;; t2 = 2 * 3
;; t3 = 4 * 2
;; 18
|#

;; 应用序
(gcd_ 206 40)

(gcd_ 40 (remainder 206 40))

(gcd_ 40 6)

(gcd_ 6 (remainder 40 6))

(gcd_ 6 4)

(gcd_ 4 (remainder 6 4))

(gcd_ 4 2)

(gcd_ 2 (remainder 4 2))

(gcd_ 2 0)

2