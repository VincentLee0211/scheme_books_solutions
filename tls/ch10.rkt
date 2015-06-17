#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date: 06/16 2015
|#

;; What Is the Value of All of This?

;; 辅助函数
(define atom? 
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define build 
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define second
  (lambda (p)
    (car (cdr p))))

(define first
  (lambda (p)
    (car p)))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else 
       (lookup-in-entry 
        name
        (car table)
        (lambda (name)
          (lookup-in-table
           name
           (cdr table)
           table-f)))))))

(define extend-table cons)

(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help
     name
     (first entry)
     (second entry)
     entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else (lookup-in-entry-help
             name
             (cdr names)
             (cdr values)
             entry-f)))))
                     


;; 求值表达式, 解释器
(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e '#t) *const)
      ((eq? e '#f) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'number?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'atom?) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) 'quote) *quote)
         ((eq? (car e) 'cond) *cond)
         ((eq? (car e) 'lambda) *lambda)
         (else *application)))
      (else *application))))

(define *application
  (lambda (e table)
    (apply_ (meaning (function-of e) table)
            (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define apply_
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive (second fun) vals))
      ((non-primitive? fun)
       (apply-closure (second fun) vals)))))

(define primitive?
  (lambda (l)
    (eq? (car l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (car l) 'non-primitive)))

(define evlis
  (lambda (args table)
    (cond
      ((null? args) '())
      (else (cons (meaning (car args) table)
                  (evlis (cdr args) table))))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'car)
       (car (first vals)))
      ((eq? name 'cdr)
       (cdr (first vals)))
      ((eq? name 'cons)
       (cons (first vals) (second vals)))
      ((eq? name 'null?)
       (null? (first vals)))
      ((eq? name 'eq?)
       (eq? (first vals) (second vals)))
      ((eq? name 'number?)
       (number? (first vals)))
      ((eq? name 'add1)
       (add1 (first vals)))
      ((eq? name 'sub1)
       (sub1 (first vals)))
      ((eq? name 'zero?)
       (zero? (first vals)))
      ((eq? name 'atom?)
       (atom? (first vals))))))
            
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table (new-entry (formals-of closure)
                                      vals)
                           (table-of vals)))))

(define body-of third)
(define formals-of second)
(define table-of first)

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive e)))))

(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))
      
(define question-of first)
(define answer-of second)

(define else?
  (lambda (x)
    (and (atom? x) (eq? x 'else))))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '())))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

;; test
(value '(add1 1))

(value '(sub1 1))

(value '(lambda (x) 
          (+ x 1)))

(value '(quote x))

#|
(meaning '((lambda (x)
            (+ x 1))
           x)
       '(((x)
          (4))))
|#

#|
(meaning '(cond 
            ((= x 3) 1)
            ((= x -3) -1)
            (else 0))
         '(((x y z)
            (4 5 6))))
|#

;; 该解释器缺少基础建设, 无法执行
;; 也就是没有顶层环境