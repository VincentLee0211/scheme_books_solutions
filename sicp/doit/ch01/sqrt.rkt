#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 求x的平方根

(define sqrt_
  (lambda (x)
    (sqrt-iter 1.0 x)))

(define sqrt-iter
  (lambda (guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x))))

(define good-enough? 
  (lambda (guess x)
    (< (abs (- (expt guess 2) x)) 0.0001)))

(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

;; test
(sqrt_ 9)

(sqrt_ (+ 100 37))

(sqrt_ (+ (sqrt_ 2) (sqrt_ 3)))

(expt (sqrt_ 1000) 2)