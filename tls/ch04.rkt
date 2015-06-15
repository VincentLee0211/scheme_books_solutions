#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date: 06/15 2015
|#

;; Numbers Games

;; 辅助函数
(define atom? 
  (lambda (x) 
    (and (not (null? x)) 
         (not (pair? x)))))

(define add1 
  (lambda (n) 
    (+ n 1)))

(define sub1 
  (lambda (n) 
    (- n 1)))

(define one? 
  (lambda (n) 
    (= n 1)))

;; (define zero? (lambda (n) (= n 0)))

(atom? 14)

(number? -3)

(number? 3.14159)

(add1 67)

(sub1 5)

;; (sub1 0)

(zero? 0)

(zero? 1492)

;; 定义加法运算
(define ^+
  (lambda (m n)
    (cond
      ((zero? m) n)
      (else (add1 (^+ (sub1 m) n))))))

(^+ 46 12)

;; 定义减法运算
(define ^-
  (lambda (m n)
    (cond
      ((zero? n) m)
      (else (sub1 (^- m (sub1 n)))))))

(^- 14 3)

(^- 17 9)

;; (^- 18 25)

;; 所有元素都是数字的列表
(define tup?
  (lambda (l)
    (cond
      ((null? l) #t)
      (else (and (number? (car l))
                 (tup? (cdr l)))))))

(tup? '(2 11 3 79 47 6))

(tup? '(8 55 5 555))

(tup? '(1 2 8 apple 4 3))

(tup? '(3 (7 4) 13 9))

(tup? '())

;; 数字列表对应项相加
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (^+ (car tup)
                (addtup (cdr tup)))))))

(addtup '(3 5 2 8))

(addtup '(15 6 7 12 3))

;; 定义乘法
(define ^*
  (lambda (m n)
    (cond
      ((zero? n) 0)
      (else (^+ m (^* m (sub1 n)))))))

(^* 5 3)

(^* 13 4)

(^* 12 3)

;; 两个数字列表相加
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (^+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(3 7 8 1)
      '(4 6))

;; 定义大于
(define ^>
  (lambda (m n)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (^> (sub1 m) (sub1 n))))))

(^> 12 133)

(^> 120 11)

(^> 123 123)

;; 定义小于
(define ^<
  (lambda (m n)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (^< (sub1 m) (sub1 n))))))

(^< 12 133)

(^< 120 11)

(^< 123 123)

;; 定义等于
#|
(define ^=
  (lambda (m n)
    (cond
      ((zero? m) (zero? n))
      ((zero? n) #f)
      (else (^= (sub1 m) (sub1 n))))))
|#

(define ^=
  (lambda (m n)
    (and (not (^< m n))
         (not (^> m n)))))

(^= 12 133)

(^= 120 11)

(^= 123 123)

;; 定义次方
(define ^^
  (lambda (b e)
    (cond
      ((zero? e) 1)
      (else (^* b (^^ b (sub1 e)))))))

(^^ 1 1)

(^^ 2 3)

(^^ 5 3)

;; 定义除法
(define ^/
  (lambda (m n)
    (cond
      ((^< m n) 0)
      (else (add1 (^/ (^- m n) n))))))

(^/ 8 3)

;; 列表长度
(define length_
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length_ (cdr lat)))))))

(length_ '(hotdogs with mustard sauerkraut and pickles))

(length_ '(ham and cheese on rye))

;; 查看列表第n个元素(从1开始)
(define pick
  (lambda (n lat)
    (cond
      ((one? n) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(pick 4 '(lasagna spaghetti ravioli macaroni meatball))

;; (pick 0 '(a))

;; 弹出列表第n个元素(从1开始)
(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n) (cdr lat)))))))

(rempick 3 '(hotdogs with hot mustard))


(number? 'tomato)

(number? 76)

;; 去除所有数字的表
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat))
       (no-nums (cdr lat)))
      (else
       (cons (car lat)
             (no-nums (cdr lat)))))))

(no-nums '(5 pears 6 prunes 9 dates))

;; 去除所有非数字的表
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat))
       (cons (car lat)
             (all-nums (cdr lat))))
      (else
       (all-nums (cdr lat))))))

(all-nums '(5 pears 6 prunes 9 dates))

;; 定义数字和原子类型相等
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (^= a1 a2))
      ((or (number? a1) (number? a2))
       #f)
      (else (eq? a1 a2)))))

;; 统计符号出现次数
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? (car lat) a)
       (add1 (occur a (cdr lat))))
      (else
       (occur a (cdr lat))))))

(occur 'c '(a b c e a b c e))