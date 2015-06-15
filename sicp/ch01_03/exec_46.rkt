#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/15 2015
|#

(define iterative-improve
  (lambda (enough improve)
    (lambda (guess)
      (let ((next (improve guess)))
        (if (enough guess next)
            next
            ((iterative-improve enough improve) next))))))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(define sqrt_
  (lambda (x)
    (define close-enough?
      (lambda (a b)
        (< (abs (- a b)) 0.0001)))
    (define improve
      (lambda (n)
        (average n (/ x n))))
    ((iterative-improve close-enough? improve) 1.0)))

(sqrt_ 2)
(sqrt_ 3)

(define  fixed-point
  (lambda (f guess)
    (define close-enough?
      (lambda (a b)
        (< (abs (- a b)) 0.0001)))
    (define improve
      (lambda (n)
        (average n (f n))))
    ((iterative-improve close-enough? improve) guess)))

(define sqrt__
  (lambda (x)
    (fixed-point (lambda (y) (average y (/ x y))) 1.0)))

(sqrt__ 2)
(sqrt__ 3)