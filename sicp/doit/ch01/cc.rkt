#|
    Author: Vincent Lee <vincentlee0211@gmail.com>
    Date:   06/12 2015
|#

;; 换零钱

(define us-coins '(50 25 10 5 1))

;; 递归计算过程
(define cc
  (lambda (amount coin-values)
    (cond
      ((= amount 0) 1)
      ((or (< amount 0)
           (no-more? coin-values)) 0)
      (else
       (+ (cc amount (except-first-denomination coin-values))
          (cc (- amount (first-denomination coin-values))
              coin-values))))))

(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)

;; ------------------------------------------------------------

(define make-table (lambda () (list '*table*)))

#|
(define assoc
  (lambda (key records)
    (cond
      ((null? records) #f)
      ((equal? key (caar records)) (car records))
      (else (assoc key (cdr records))))))
|#

(define lookup
  (lambda (key table)
    (let ((record (assoc key (cdr table))))
      (if record 
          (cdr record)
          #f))))

(define insert!
  (lambda (key value table)
    (let ((record (assoc key (cdr table))))
      (if record
          (set-cdr! record value)
          (set-cdr! table 
                    (cons (cons key value) (cdr table)))))))

(define local-table (make-table))

(define memoize
  (lambda (f)
    (let ((table local-table))
      (lambda (amount coin-values)
        (let ((previously-computed-result (lookup (cons amount coin-values) table)))
          (or previously-computed-result
              (let ((result (f amount coin-values)))
                (insert! (cons amount coin-values) result table)
                result)))))))

;; 迭代计算过程
;; 带记忆的过程, 参见3.27
(define memo-cc
  (memoize (lambda (amount coin-values)
             (cond
               ((= amount 0) 1)
               ((or (< amount 0)
                    (no-more? coin-values)) 0)
               (else
                (+ (memo-cc amount (except-first-denomination coin-values))
                   (memo-cc (- amount (first-denomination coin-values)) coin-values)))))))

