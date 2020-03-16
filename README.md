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
        ((eq a (car lst)) (checker (cdr lst) a))
        (t (cons (car lst) (checker (cdr lst) a)))
        
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
 21. Определите функцию, удаляющую из списка первое вхождение данного элемента на верхнем уровне.
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

25. Определите функцию, удаляющую из списка каждый четный элемен

```lisp
(defun delete-even-number (x )
    (cond
        ((null x)nil)
        ((eq (rem (car x) 2) 0) (delete-even-number (cdr x)))
        (t (cons (car x) (delete-even-number (cdr x))))
    )
)
```
Пример

```lisp
(delete-even-number '(1 2 3 4 5 6 7 8 9 10))
    (1 3 5 7 9) 
    
(delete-even-number '(2 2 2 )))
    NIL 

(delete-even-number '(1 1 1  )))
    (1 1 1)
```

28. Определите функцию, вычисляющую, сколько всего атомов в списке (списочной структуре).

```lisp
(defun atom-cnt (lst) 
    (cond
        ((null lst) 0)
        ((eq nil (car lst)) (atom-cnt (cdr lst)))
        ((atom (car lst)) (+ 1 (atom-cnt (cdr lst))))
        (t (+(atom-cnt(cdr lst)) (atom-cnt (car lst))))
    )
)
```
Пример

```lisp
(atom-cnt '(1 2 3 (1 2 3 (1 2 3)) 1 2 (1)))
    11
    
(atom-cnt '())
    0

(atom-cnt '(1 () (1)))
    2
```
32. Определите предикат МНОЖЕСТВО-Р, который проверяет, является ли список множеством, т.е. входит ли каждый элемент в список лишь один раз

```lisp

```

Пример
```lisp
```

42. Определите функцию, находящую максимальное из значений, находящихся ввершинах деревa.

```lisp

```


Пример

```lisp

```

45. Предположим, что у имени города есть свойства х и у, которые содержат координаты места нахождения города относительно некоторого начала координат.Напишите функцию(РАССТОЯНИЕ a b), вычисляющую расстояние между городами а и b.

```lisp

```
Пример

```lisp

```
