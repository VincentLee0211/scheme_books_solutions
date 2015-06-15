#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

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
        (< (abs (- x y)) 0.0001)))
    (try guess)))

;; 求黄金分割率

(define thita
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

thita