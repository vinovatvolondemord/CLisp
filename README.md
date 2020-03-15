8. Определите функцию, которая разделит исходный список из целых чисел надва списка: список положительных чисел и список отрицательных чисел.

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

13. Определите функцию, удаляющую в исходном списке все повторные вхожде-ния элементов.
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
    (1 2 3 5 4)

(1 1 1 1 1 1  1)
    (1)
```

15. Определите функцию, вычисляющую скалярное произведение векторов, за-данных списками целых чисел

```lisp
(defun scalar (a b) 
    (
        (lambda (ha ta hb tb)
            (cond
                ((OR (null ha) (null hb)) nil)
                ((OR (atom ta) (atom tb)) (* ha hb))
                (t (+ (* ha hb) (scalar ta tb)))
            )
        )(car a) (cdr a) (car b) (cdr b)
    )
)
```
 Пример
 
 ```lisp
A(1 2 4) B(1 2 4)
    21
  
A(1) B(2)
    2

A() B(2 3 1)
    NIL

A(1) B(1 2)
    1
 ```
 21. Определите функцию, удаляющую из списка первое вхождение данного эле-мента на верхнем уровне.
 ```lisp
 
(defun delete-first-occurrence (lst a)
    (cond 
        ((null lst) nil)
        ((eq nil (car lst))(delete-first-occurrence (cdr lst) a))
        ((atom (car lst)) 
            (cond
                ((/= a (car lst)) (cons (car lst) (delete-first-occurrence (cdr lst) a)))
                ((= a (car lst)) (cdr lst))
            )
        )
        (t (delete-first-occurrence (cdr lst) a))
    )
)
```
Пример

```lisp
(delete-first-occurrence '(1 2 3 () 1 2 3) 2)
    (1 3 NIL 1 2 3)
    
(delete-first-occurrence '(1 2 3 (1 2 3 ) 1 2 3) 2)
    (1 3 (1 2 3) 1 2 3)
```
