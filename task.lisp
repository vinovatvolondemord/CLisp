
;-----------------------------------------------------------------------
;(проверено) 8. Определите функцию, которая разделит исходный список из целых чисел надва списка: 
;список положительных чисел и список отрицательных чисел.


(defun split (lst)
    (cond 
        ((null lst) nil)
        (T
         ((lambda (first restl crestl)
          (cond
           ( (> first 0) (cons (cons first crestl) (cdr restl)))
           (t (list crestl (cons first (cadr restl))))
          )
         )(car lst)(split(cdr lst))(car (split(cdr lst)))))
    )
)

;-----------------------------------------------------------------------

(print '(Задача № 8))
(print (split '(-2 2 3 4 -2 -1)))
(print (split '(-2 -2 -1)))


;-----------------------------------------------------------------------
;(проверено) 13. Определите функцию, удаляющую в исходном списке все повторные вхожде-ния элементов.

(defun delete-similar (lst)
    ((lambda (a b)
         (cond
            ((null lst) nil)
            (t  (cons a (delete-similar (checker b a)))) 
        )
    )(car lst)(cdr lst))
)

(defun checker (lst a)
    (cond 
        ((null lst) nil)
        (T  ((lambda (c b)
            (cond
                ((eq a c) b)
                (t (cons c b)))
        )(car lst)(checker (cdr lst) a)))
    )
)

;-----------------------------------------------------------------------
(print '(Задача № 13))
(print (delete-similar '(1 2 2 1)))
(print (delete-similar '(1 2 2 3 5 1 4 5 1)))
(print (delete-similar '(1 1 1 1 1 1  1)))


;-----------------------------------------------------------------------
; (проверено)15. Определите функцию, вычисляющую скалярное произведение векторов,
;за-данных списками целых чисел

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
(print '(Задача № 15))
(print (scalar '(1 23 3) '(2 3)))
(print (scalar '(1 2) '(2 1)))
(print (scalar '() '(1)))

;-----------------------------------------------------------------------
 ;(исправлено) 21. Определите функцию, удаляющую из списка первое вхождение данного элемента
 ;на верхнем уровне.

 
(defun delete-first-occurrence (lst a)
    (cond 
        ((null lst) nil)
        (T 
         ((lambda (b c d)
          (cond
           ((eq a b)c)
           (t (cons b d)))
         )(car lst)(cdr lst)(delete-first-occurrence (cdr lst) a)))
    )
)




;-----------------------------------------------------------------------
(print '(Задача № 21))
(print (delete-first-occurrence '(1 2 3 () 1 2 3) 2))   
(print (delete-first-occurrence '(1 2 3 (1 2 3 ) 1 2 3) 2))


;-----------------------------------------------------------------------
; 25.(не знаю как упростить) Определите функцию, удаляющую из списка каждый четный элемен

(defun delete-even-number (x )
    (cond
        ((null x)nil)
        (T((lambda (a b)
           (cond
            ((eq (rem a 2) 0) b)
            (t (cons a b))
        )
     )(car x)(delete-even-number(cdr x))))
    )
)

;-----------------------------------------------------------------------
(print '(Задача №25))
(print (delete-even-number '(1 2 3 4 5 6 7 8 9 10)))  
(print (delete-even-number '(2 2 2 )))
(print (delete-even-number '(1 1 1  )))

;-----------------------------------------------------------------------
;(исправлено)28. Определите функцию, вычисляющую, сколько всего атомов в списке (списочной структуре).

(defun atom-cnt (lst) 
    (cond
        ((null lst) 0)
        (T((lambda (a b)
         (cond
          ((atom a) (+ 1 b))
          (t (+ b (atom-cnt a)))
         )
        )(car lst)(atom-cnt(cdr lst))))
    )
)


;-----------------------------------------------------------------------
(print '(Задача № 28))
(print (atom-cnt '(1 2 3 (1 2 3 (1 2 3)) 1 2 (1))))   
(print (atom-cnt '()))
(print (atom-cnt '(1 () (1))))

;-----------------------------------------------------------------------
;(исправлено) 32. Определите предикат МНОЖЕСТВО-Р, который проверяет, является ли
;список множеством, т.е. входит ли каждый элемент в список лишь один раз



(defun МНОЖЕСТВО-Р (x)
    ((lambda (a b)
        (cond
            ((null x) t)
            ((member1 b a) nil)
            (t (МНОЖЕСТВО-Р b))
        )
     )(car x)(cdr x))
)

(defun member1 (lst a)
    (cond 
        ((null lst) nil)
        ((eq a (car lst)) t)
        (t (member1 (cdr lst) a))
    )
)



;-----------------------------------------------------------------------
(print '(Задача № 32))
(print (МНОЖЕСТВО-Р '(1 2 11 111 1)))
(print (МНОЖЕСТВО-Р '(1 2 11 111 )))


;-----------------------------------------------------------------------

;(проверено)42. Определите функцию, находящую максимальное из значений, находящихся в
;1вершинах деревa.


(defun biggest-tree-node (tree)
    (cond
        ((null tree) 0)
        (T((lambda (a b)
           (cond
                ((atom a) (max (car tree) b))
                ((atom (caar tree)) (max (biggest-tree-node a) b))
           )
       )(car tree) (biggest-tree-node (cdr tree))))
    )
)

;-----------------------------------------------------------------------
(print '(Задача № 42))
(print (biggest-tree-node '(1 (3 (5)(1000000)) (100 (24444) (44444)))))
(print (biggest-tree-node '(1 (3 (5)(1)) (100 (24444) (44444)))))
(print (biggest-tree-node '(1 (3 (5)(1)) (100 (24444) (1)))))
(print (biggest-tree-node '(1 (3 (5)(1)) (100 (1) (1)))))
(print (biggest-tree-node '(1 (3 (5)(1)) (1 (1) (1)))))
        


;-----------------------------------------------------------------------
;(не готово) 45. Предположим, что у имени города есть свойства х и у, которые содержат 
;координаты места нахождения города относительно некоторого начала 
;координат.Напишите функцию(РАССТОЯНИЕ a b), вычисляющую расстояние между городами а и b.


(defun range-bitween-cities (first second)
    ((lambda (xfs yfs)
        (cond
            ((null first) nil)
            (t (sqrt (+ (* xfs xfs)(* yfs yfs))))
        )
    )(- (car first)(car second))(- (cadr first)(cadr second)))
)



;-----------------------------------------------------------------------
(print '(Задача № 45))
(set 'simf '(1.0002 1.0003))
(set 'sevast '(1 2))

(print(range-bitween-cities simf sevast))

;(не готово)46.Предположим, что отец и мать некоторого лица, хранятся как значения соответствующих
;свойств у символа, обозначающего это лицо. Напишите функцию (РОДИТЕЛИ x), которая
;возвращает в качестве значения родителей, и предикат(СЕСТРЫ-БРАТЬЯ x1 x2), который
;истинен в случае, если x1 и x2 — сестры или  братья, родные или с одним общим родителем.
defun get-mother(x)
    (get x 'mother)
)

(defun get-father(x)
    (get x 'father)
)

(defun parents(x)
    (list (get-mother x) (get-father x))
)

(defun set-parents(x mother father)
    (setf (get x 'mother) mother)
    (setf (get x 'father) father)
)

(defun sisters-brothers(x y)
    (cond
        ((eq (get-mother x) (get-mother y))   T)
        ((eq (get-father x) (get-father y))   T)
        (   T                                   NIL)
    )
)

;----------------------------------------------------------
(set-parents 'I 'A 'B)
(set-parents 'G 'A 'B)
(set-parents 'K 'A 'B)



