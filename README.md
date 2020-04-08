
```lisp

;-----------------------------------------------------------------------
; 8. Определите функцию, которая разделит исходный список из целых чисел надва списка: список положительных чисел и список отрицательных чисел.


(defun split (lst)
    ((lambda (first restl)
        (cond 
            ((null lst) nil)
            ( (> first 0) (cons (cons first (car (split restl))) (cdr (split restl))))
            (t (list (car (split restl)) (cons first (cadr (split restl)))))
        )
    )(car lst)(cdr lst)
))

;-----------------------------------------------------------------------
(print (split '(-2 2 3 4 -2 -1)))
(print (split '(-2 -2 -1)))


;-----------------------------------------------------------------------
; 13. Определите функцию, удаляющую в исходном списке все повторные вхожде-ния элементов.

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

;-----------------------------------------------------------------------
(print (delete-similar '(1 2 2 1)))
(print (delete-similar '(1 2 2 3 5 1 4 5 1)))
(print (delete-similar '(1 1 1 1 1 1  1)))


;-----------------------------------------------------------------------
; 15. Определите функцию, вычисляющую скалярное произведение векторов, за-данных списками целых чисел

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

;-----------------------------------------------------------------------
(print (scalar '(1 23 3) '(2 3)))
(print (scalar '(1 2) '(2 1)))
(print (scalar '() '(1)))

;-----------------------------------------------------------------------
 ; 21. Определите функцию, удаляющую из списка первое вхождение данного элемента на верхнем уровне.

 
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


;-----------------------------------------------------------------------
(print (delete-first-occurrence '(1 2 3 () 1 2 3) 2))   
(print (delete-first-occurrence '(1 2 3 (1 2 3 ) 1 2 3) 2))


;-----------------------------------------------------------------------
; 25. Определите функцию, удаляющую из списка каждый четный элемен

(defun delete-even-number (x )
    (cond
        ((null x)nil)
        ((eq (rem (car x) 2) 0) (delete-even-number (cdr x)))
        (t (cons (car x) (delete-even-number (cdr x))))
    )
)


;-----------------------------------------------------------------------
(print (delete-even-number '(1 2 3 4 5 6 7 8 9 10)))  
(print (delete-even-number '(2 2 2 )))
(print (delete-even-number '(1 1 1  )))

;-----------------------------------------------------------------------
;28. Определите функцию, вычисляющую, сколько всего атомов в списке (списочной структуре).


(defun atom-cnt (lst) 
    (cond
        ((null lst) 0)
        ((eq nil (car lst)) (atom-cnt (cdr lst)))
        ((atom (car lst)) (+ 1 (atom-cnt (cdr lst))))
        (t (+(atom-cnt(cdr lst)) (atom-cnt (car lst))))
    )
)


;-----------------------------------------------------------------------
(print (atom-cnt '(1 2 3 (1 2 3 (1 2 3)) 1 2 (1))))   
(print (atom-cnt '()))
(print (atom-cnt '(1 () (1))))

;-----------------------------------------------------------------------
; 32. Определите предикат МНОЖЕСТВО-Р, который проверяет, является ли список множеством, т.е. входит ли каждый элемент в список лишь один раз


(defun МНОЖЕСТВО-Р (x)
    (cond
        ((null x) t)
        ((member (car x) (cdr x)) nil)
        (t (МНОЖЕСТВО-Р (cdr x)))
    )
)


;-----------------------------------------------------------------------
(print (МНОЖЕСТВО-Р '(1 2 11 111 1)))
(print (МНОЖЕСТВО-Р '(1 2 11 111 )))


;-----------------------------------------------------------------------

;42. Определите функцию, находящую максимальное из значений, находящихся в 1вершинах деревa.


(defun biggest-tree-node (tree)
    (cond
        ((null tree) 0)
        ((atom (car tree)) (max (car tree) (biggest-tree-node (cdr tree))))
        ((atom (caar tree)) (max (biggest-tree-node (car tree)) (biggest-tree-node (cdr tree))))
    )
)

;-----------------------------------------------------------------------

(print (biggest-tree-node '(1 (3 (5)(1000000)) (100 (24444) (44444)))))
(print (biggest-tree-node '(1 (3 (5)(1)) (100 (24444) (44444)))))
(print (biggest-tree-node '(1 (3 (5)(1)) (100 (24444) (1)))))
(print (biggest-tree-node '(1 (3 (5)(1)) (100 (1) (1)))))
(print (biggest-tree-node '(1 (3 (5)(1)) (1 (1) (1)))))
        


;-----------------------------------------------------------------------
; 45. Предположим, что у имени города есть свойства х и у, которые содержат координаты места нахождения города относительно некоторого начала координат.Напишите функцию(РАССТОЯНИЕ a b), вычисляющую расстояние между городами а и b.


(defun range-bitween-cities (first second)
    ((lambda (xfs yfs)
        (cond
            ((null first) nil)
            (t (sqrt (+ (* xfs xfs)(* yfs yfs))))
        )
    )(- (car first)(car second))(- (cadr first)(cadr second)))
)



;-----------------------------------------------------------------------

(set 'simf '(1.0002 1.0003))
(set 'sevast '(1 2))

(print(range-bitween-cities simf sevast))

   
```
# 46. Предположим, что отец и мать некоторого лица, хранятся как значения соответствующих свойств у символа, обозначающего это лицо. Напишите функцию(РОДИТЕЛИ x), которая возвращает в качестве значения родителей, и предикат(СЕСТРЫ-БРАТЬЯ x1 x2), который истинен в случае, если x1 и x2 сестры или братья, родные или с одним общим родителем

```lisp

```
Пример

```lisp

```
