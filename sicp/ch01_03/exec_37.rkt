#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

;; 无穷连分式

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

;; 递归计算过程
(define cont-frac
  (lambda (n d k)
    (define recv
      (lambda (count)
        (if (= count k)
            (/ (n count) (d count))
            (/ (n count)
               (+ (d count) (recv (+ count 1)))))))
    (recv 1)))

;; 迭代计算过程
(define cont-frac_
  (lambda (n d k)
    (define iter
      (lambda (init count)
        (if (= count 0)
            init
            (iter (/ (n count) (+ (d count) init))
                  (- count 1)))))
    (iter (/ (n k) (d k)) (- k 1))))

;; test
(cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0)
           1000)

(cont-frac_ (lambda (x) 1.0)
            (lambda (x) 1.0)
            1000)