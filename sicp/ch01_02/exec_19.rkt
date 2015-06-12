#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 设计一个快速求fib的过程

(define fib
  (lambda (n)
    (define iter
      (lambda (a b p q count)
        (cond
          ((= count 0) b)
          ((even? count)
           (iter a
                 b
                 (+ (expt p 2) (expt q 2))
                 (+ (expt q 2) (* 2 p q))
                 (/ count 2)))
          (else
           (iter (+ (* b q) (* a q) (* a p))
                 (+ (* b p) (* a q))
                 p
                 q
                 (- count 1))))))
    (iter 1 0 0 1 n)))



;; T(p, q), 其中p = 0, q = 1
;; a <- bq + aq + ap
;; b <- bp + aq

;; 二次变换之后
;; a <- (bp+aq)q + (bq+aq+ap)q + (bq+aq+ap)p
;;   <- bpq + aqq + bqq + aqq + apq + bpq + app
;;   <- b(2pq+qq) + a(2pq+qq) + a(pp+qq)

;; b <- (bp+aq)p + (bq+aq+ap)q
;;   <- bpp + apq + bqq + aqq + apq
;;   <- b(pp+qq) + a(qq + 2pq)

;; 从上面得知, 经两次转换之后
;; p <- pp + qq
;; q <- qq + 2pq