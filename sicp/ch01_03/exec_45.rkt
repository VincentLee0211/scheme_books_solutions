#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/15 2015
|#

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(define average-damp
  (lambda (f)
    (lambda (x)
      (average x (f x)))))
           
(define fixed-point
  (lambda (f guess)
    (define try
      (lambda (guess)
        (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next)))))
    (define close-enough?
      (lambda (x y)
        (< (abs (- x y)) 0.000000001)))
    (try guess)))

(define repeated
  (lambda (f times)
    (lambda (x)
      (if (= times 1)
          (f x)
          (f ((repeated f (- times 1)) x))))))

(define nth
  (lambda (x n)
    (fixed-point 
     (repeated 
      (average-damp 
       (lambda (y) 
         (/ x (expt y (- n 1)))))
      (how-times n))
     1.0)))

(define nth
  (lambda (x n)
    (fixed-point ((repeated average-damp (how-times n))
                  (lambda (y) (/ x (expt y (- n 1)))))
                 1.0)))

#|
(define how-times
  (lambda (n)
    3))
|#

;; (log2) n 向下取整
;; (floor (/ (log n) (log 2)))

(define how-times
  (lambda (n)
    (floor (/ (log n) (log 2)))))

;; n = 2 ~ 3    1    2^1
(nth 2 2)
(nth 2 3)

;; n = 4 ~ 7    2    2^2
(nth 2 4)
(nth 2 5)
(nth 2 7)

;; n = 8 ~ 15   3    2^3
(nth 2 8)
(nth 2 15)

(nth 2 16)
(nth 2 32)
(nth 2 33)
(nth 2 64)
(nth 2 100)

;; 该问题的问题在于, 当close-enough?的精度要求不高的时候, 在高等级的求次方时, 总是会返回值
;; 因为值的变动范围已经比close-enough?要求的精度还要小
