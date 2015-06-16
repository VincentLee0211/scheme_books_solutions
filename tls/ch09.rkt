#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date: 06/16 2015
|#

;; ... and Again, and Again, and Again, ...

;; 辅助函数
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))

(define one?
  (lambda (x)
    (= x 1)))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking 
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))

(define pick
  (lambda (n lat)
    (cond
      ((one? n) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(looking 'caviar 
         '(6 2 4 caviar 5 7 3))

(looking 'caviar
         '(6 2 grits caviar 5 7 3))
         
;; partial function
;; total function

;; 无限
(define eternity
  (lambda (x)
    (eternity x)))

;; 辅助函数
(define first
  (lambda (pair)
    (car pair)))

(define second
  (lambda (pair)
    (car (cdr pair))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))


;; 左移
(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(shift '((a b) c))

(shift '((a b) (c d)))

;; 
(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((pair? (first pora))
       (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))

;; 
(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (length* (first pora))
               (length* (second pora)))))))

;;
(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (* 2 (weight* (first pora)))
               (weight* (second pora)))))))

(weight* '((a b) c))

(weight* '(a (b c)))

;; 辅助函数
(define revpair
  (lambda (pair)
    (build (second pair)
           (first pair))))
           
;; 
(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))

(shuffle '(a (b c)))

(shuffle '(a b))

;; (shuffle '((a b) (c d)))

(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      ((even? n) (C (/ n 2)))
      (else (C (add1 (* 3 n)))))))

(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))

;; 停机问题

;; 假设存在一个函数, 已另一个函数为参数
(define will-stop?
  (lambda (f)
    ...))

;; 当函数可以停机时, 返回#t; 不能停机, 返回#f

;; 定义函数last-try
(define last-try
  (lambda (x)
    (and (will-stop? last-try)
         (eternity x))))

;; 假设last-try可以停机
;; 1. will-stop?返回#t
;; 2. 根据and的求值规则, 需要计算eternity函数
;; 3. 此时last-try无法停机(陷入无穷递归中)
;; 4. 则假设不成立

;; 假设last-try不可以停机
;; 1. will-stop?返回#f
;; 2. 根据and的求值规则, 直接返回#f
;; 3. 此时说明last-try可以停机
;; 4. 则假设不成立

;; 由上述可知, will-stop?函数不存在

;; Y combinator
(define length_
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length_ (cdr l)))))))

;; 假设不存在(define ...), 则此时的(lambda ...)无法命名
;; 如何引用呢?

;; length 0
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))

;; length 1
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (length0 (cdr l))))))

(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 ((lambda (l)
                   (cond
                     ((null? l) 0)
                     (else (add1 (eternity (cdr l))))))
                 (cdr l))))))

;; length 2
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 ((lambda (l)
                   (cond
                     ((null? l) 0)
                     (else (add1 ((lambda (l)
                                    (cond
                                      ((null? l) 0)
                                      (else (add1 (eterity (cdr l))))))
                                  (cdr l))))))
                 (cdr l))))))

;; 使用抽象化, 抽象出共同模式

;; length 0
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)

;; length 1
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
  eternity))

;; length 2
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
   eternity)))

;; 进一步抽象

;; length 0
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;; length 1
((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;; length 2
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;; length 3
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length 
      (mk-length eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;; 传递的终结函数, 是无所谓的
;; 此处可以用mk-length替换eternity

;; length 0
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
    
;; 亦可以用mk-length替换length

;; length 0
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (mk-length (cdr l))))))))

;; length 1
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length eternity) (cdr l))))))))

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((mk-length eternity) (cdr l))))))))
 '(apples))

;; length
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length mk-length)
                    (cdr l))))))))

(((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length mk-length)
                    (cdr l))))))))
 '(a b c d e f g))

;; 上面的计算函数, 为发现(length)定义

#|
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    (mk-length mk-length))))
|#

#|
(f x) = (lambda (x) (f x))

(mk-length mk-length) 

(lambda (x)
  ((mk-length mk-length) x))
|#

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((lambda (x)
                      ((mk-length mk-length) x))
                    (cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))

;; 进一步抽象
((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;; 将模式提取
(lambda (le)
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x)
           ((mk-length mk-length) x))))))

;; 为模式命名
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))