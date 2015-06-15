#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date: 06/15 2015
|#

;; Do It, Do It Again, and Again, and Again ...

;; 辅助函数
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define add1 (lambda (x) (+ x 1)))
(define sub1 (lambda (x) (- x 1)))

;; 列表中任何一个元素都是原子类型
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      (else (and (atom? (car l)) (lat? (cdr l)))))))

(lat? (quote (Jack Sprat could eat no chicken fat)))

(lat? (quote ((Jack) Sprat could eat no chicken fat)))

(lat? (quote (Jack (Sprat could) eat no chicken fat)))

(lat? (quote ()))

(lat? (quote (bacon and eggs)))

(lat? (quote (bacon (and eggs))))

;; 验证元素是列表元素
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(member? (quote meat)
         (quote (mashed potatoes and meat gravy)))

(member? (quote liver)
         (quote (bagels and lox)))
                     