#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 创建函数f, 使用递归和迭代表达

(define f1
  (lambda (n)
    (if (< n 3)
        n
        (+ (f1 (- n 1))
           (* 2 (f1 (- n 2)))
           (* 3 (f1 (- n 3)))))))

(define f2
  (lambda (n)
    (define iter
      (lambda (a b c count)
        (if (= count 3)
            #|
            (+ a (* 2 b) (* 3 c))
            (iter (+ a (* 2 b) (* 3 c))
                  a 
                  b
                  (- count 1)))))
           |#
            a
            (iter (+ a (* 2 b) (* 3 c))
                  a
                  b
                  (- count 1)))))
    (if (< n 3)
        n
        ;; (iter 2 1 0 n))))
        (iter 4 2 1 n))))