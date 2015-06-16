#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date: 06/16 2015
|#

;; Shadows

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

;; 定义 ^+ ^* ^^ 
(define ^+
  (lambda (m n)
    (cond
      ((zero? n) m)
      (else (add1 (^+ m (sub1 n)))))))

(define ^*
  (lambda (m n)
    (cond
      ((zero? n) 0)
      (else (^+ m (^* m (sub1 n)))))))

(define ^^
  (lambda (m n)
    (cond
      ((zero? n) 1)
      (else (^* m (^^ m (sub1 n)))))))

;; 定义数学表达式
#|
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) '^+)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '^*)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '^^)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))))))
|#
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp))
                 (numbered? (car (cdr (cdr aexp)))))))))

(numbered? 1)

(numbered? 3)

(numbered? '(1 ^+ 3))

(numbered? '(1 ^+ (3 ^* 4)))

(numbered? '((3 ^^ 3) ^+ 5))

(quote a)

(quote ^+)

(quote ^*)

(eq? (quote a) 'a)

(eq? 'a 'a)
       
;; 求算术表达式的值
(define value
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (car (cdr aexp)) '^+)
       (^+ (value (car aexp))
           (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '^*)
       (^* (value (car aexp))
           (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '^^)
       (^^ (value (car aexp))
           (value (car (cdr (cdr aexp)))))))))

(value 13)

(value '(1 ^+ 3))

(value '(1 ^+ (3 ^^ 4)))

;; 定义前缀表达式算术运算
(define value_
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (car aexp) '^+)
       (^+ (value_ (car (cdr aexp)))
           (value_ (car (cdr (cdr aexp))))))
      ((eq? (car aexp) '^*)
       (^* (value_ (car (cdr aexp)))
           (value_ (car (cdr (cdr aexp))))))
      ((eq? (car aexp) '^^)
       (^^ (value_ (car (cdr aexp)))
           (value_ (car (cdr (cdr aexp)))))))))

;; 辅助函数
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value_
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (operator aexp) '^+)
       (^+ (value_ (1st-sub-exp aexp))
           (value_ (2nd-sub-exp aexp))))
      ((eq? (operator aexp) '^*)
       (^* (value_ (1st-sub-exp aexp))
         (value_ (2nd-sub-exp aexp))))
      ((eq? (operator aexp) '^^)
       (^^ (value_ (1st-sub-exp aexp))
           (value_ (2nd-sub-exp aexp)))))))

(value_ '(^+ 3 3))

(value_ '(^+ 3 (^^ 5 2)))

;; 数字 number? zero? add1 sub1
;; 用列表表示数字 sero? edd1 zub1
(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define $+
  (lambda (m n)
    (cond
      ((sero? n) m)
      (else (edd1 ($+ m (zub1)))))))


