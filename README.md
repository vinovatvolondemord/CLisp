8.пределите функцию, которая разделит исходный список из целых чисел надва списка: список положительных чисел и список отрицательных чисел.

```lisp
(defun split (lst)
    ((lambda (first restl)
        (cond 
            ((null lst) nil)
            ( (> first 0) (cons (cons first (car (split restl))) (cdr (split restl))))
            (t (list (car (split restl)) (cons first (cadr (split restl)))))
        )
    )(car lst)(cdr lst)
))

```
Примеры

```lisp

(-2 2 3 4 -2 -1)
    ((2 3 4) (-2 -2 -1))

(-2 -2 -1)
    (NIL (-2 -2 -1))
```

13.Определите функцию, удаляющую в исходном списке все повторные вхожде-ния элементов.
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
Пример

```lisp
(1 2 2 1)
    (1 2)

(1 2 2 3 5 1 4 5 1)
    (1 2 3 4 5)

(1 1 1 1 1 1  1)
    (1)
```
