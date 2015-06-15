#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

(define cube (lambda (x) (expt x 3)))

;; 求和
(define sum
  (lambda (term a next b)
    (if (> a b)
        0
        (+ (term a) (sum term (next a) next b)))))

;; 用辛普森规则求定积分
(define integral
  (lambda (f a b n)
    (let ((h (/ (- b a) n)))
      (* (/ h 3.0)
         (sum (lambda (x)
                (cond
                  ((= x 0) (f a))
                  ((= x n) (f b))
                  ((even? x) (* 2 (f (+ a (* x h)))))
                  (else (* 4 (f (+ a (* x h)))))))
              0
              (lambda (x) (+ x 1))
              n)))))

(integral cube 0 1 100)

(integral cube 0 1 1000)