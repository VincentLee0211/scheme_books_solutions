#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/13 2015
|#

;; 重复调用函数

(define repeated
  (lambda (f times)
    (lambda (x)
      (if (= times 1)
          (f x)
          (f ((repeated f (- times 1)) x))))))

(define square (lambda (x) (expt x 2)))

((repeated square 2) 5)

(define repeated_
  (lambda (f times)
    (lambda (x)
      (define iter
        (lambda (times-remaining result)
          (if (= times-remaining 1)
              result
              (iter (- times-remaining 1) (f result)))))
      (iter times (f x)))))

((repeated_ square 2) 5)