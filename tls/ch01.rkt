#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date: 06/15 2015
|#

;; Toys

;; 辅助函数
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define add1 (lambda (x) (+ x 1)))
(define sub1 (lambda (x) (- x 1)))

(define length_
  (lambda (lat)
    (if (null? lat)
        0
        (add1 (length (cdr lat))))))

(atom? (quote atom))

(atom? (quote atom))

(atom? 1492)

(atom? (quote u))

(atom? (quote *abc$))

(pair? (quote (atom)))

(pair? (quote (atom turkey or)))

;; 非法表达式
;; (pair? (quote (atom turkey) or))

(pair? (quote ((atom turkey) or)))

(atom? (quote xyz))

(pair? (quote (x y z)))

(pair? (quote ((x y) z)))

(pair? (quote (how are you doing so far)))

(length (quote (how are you doing so far)))

(pair? (quote (((how) are) ((you) (doing so)) far)))

(length (quote (((how) are) ((you) (doing so)) far)))

(null? (quote ()))
(pair? (quote ()))

(atom? (quote ()))

(pair? (quote (() () () ())))

(car (quote (a b c)))

(car (quote ((a b c) x y z)))

;; (car (quote hotdog))

;; (car (quote ()))

(car (quote (((hotdogs)) (and) (pickle) relish)))

(car (car (quote (((hotdogs)) (and)))))

(cdr (quote (a b c)))

(cdr (quote ((a b c) x y z)))

(cdr (quote (hamburger)))

(cdr (quote ((x) t r)))

;; (cdr (quote hotdogs))

;; (cdr (quote ()))

(car (cdr (quote ((b) (x y) ((c))))))

(cdr (cdr (quote ((b) (x y) ((c))))))

;; (cdr (car (quote (a (b (c)) d))))

(cons (quote peanut)
      (quote (butter and jelly)))

(cons (quote (banana and))
      (quote (peanut butter and jelly)))

(cons (quote ((help) this))
      (quote (is very ((hard) to learn))))

(cons (quote (a b (c))) (quote ()))

(cons (quote a) (quote ()))

;; 可以生成序对
;; (cons (quote ((a b c))) (quote b))

;; (cons (quote a) (quote b))

(cons (quote a) 
      (car (quote ((b) c d))))
          
(cons (quote a)
      (cdr (quote ((b) c d))))

(null? (quote ()))

(null? (quote (a b c)))

;; 可以判断原子类型
;; (null? (quote spaghetti))

(atom? (quote Harry))

(atom? (quote (Harry had a heap of apples)))

(atom? (car (quote (Harry had a heap of apples))))

(atom? (cdr (quote (Harry had a heap of apples))))

(atom? (cdr (quote (Harry))))

(atom? (car (cdr (quote (swing low sweet cherry oat)))))

(atom? (car (cdr (quote (swing (low sweet) cherry oat)))))

(eq? (quote Harry) (quote Harry))

(eq? (quote margarine) (quote butter))

;; (eq? (quote ()) (quote (strawberry)))

;; 参考具体实现, 如果只说语法, 可以认为无法判定
;; (eq? 6 7)

(eq? (car (quote (Mary had a little lamb chop)))
     (quote Mary))

;; (eq? (cdr (quote (soured milk))) (quote milk))

(eq? (car (quote (beans beans we need jelly beans)))
     (car (cdr (quote (beans beans we need jelly beans)))))