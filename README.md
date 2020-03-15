8.пределите функцию, которая разделит исходный список из целых чисел надва списка: список положительных чисел и список отрицательных чисел.

```lisp

(defun func (lst &optional pos neg) 
    ((lambda (a b c d)
             (cond 
                 ((null c) (list a b ))
                 (t ( if (> 0 c) ( func d (cons c a) b ) (func d a (cons c b) ))))
             ) pos neg (car lst) (cdr lst)))




(print (func '(2 3 4 0 -2 -3 4)))  
(print (func '( 2 3 4)))  
(print (func '(-2 -2 -2)))
```
((-3 -2) (4 0 4 3 2)) 

(NIL (4 3 2)) 

((-2 -2 -2) NIL)


(setq lst '(( 2 3) (-3 -4)))

;(print (cons (cons 1 (car lst)) (cdr lst)))
;(print (list(car lst) (cons 1 (cadr lst))))
(defun split (lst)
    (cond 
        ((null lst) nil)
        (t (lambda (first restl)( (> (first ) 0) 
             (cons (cons (car lst) (car (split (cdr lst)))) (cdr (split (cdr lst)))))
        (t 
             (list(car (split (cdr lst))) (cons (car lst) (cadr (split (cdr lst))))))
        ))
(print (split '(2 3 4 -2 -1)))

```lisp
(defun delete-similar (lst)
    (cond
        ((null lst) nil)
        (t  (cons (car lst) (delete-similar (checker (cdr lst) (car lst))))) 
    )
)

(defun checker (lst a)
    (cond 
        ((null lst) nil)
        ((/= a (car lst)) (cons (car lst) (checker (cdr lst) a)))
        ((= a (car lst)) (checker (cdr lst) a))
    )
)
```
