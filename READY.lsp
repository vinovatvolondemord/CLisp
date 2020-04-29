; 25.Определите функцию, удаляющую из списка каждый четный элемен



(defun delete-even-element (x )
    (cond
        ((null x)nil)
        (t (cons (car x) (delete-even-element(cddr x))))
    )
)
;=========================================
(print (delete-even-element '(1)))
(print (delete-even-element '(1 2 3 4 5 6)))

;========================================


;46.Предположим, что отец и мать некоторого лица, хранятся как значения соответствующих
;свойств у символа, обозначающего это лицо. Напишите функцию (РОДИТЕЛИ x), которая
;возвращает в качестве значения родителей, и предикат(СЕСТРЫ-БРАТЬЯ x1 x2), который
;истинен в случае, если x1 и x2 — сестры или  братья, родные или с одним общим родителем.


(defun set-parents(x mother father)
    (setf (get x 'mother) mother)
    (setf (get x 'father) father)
)

(defun sisters-brothers(x y)
    (cond
        ((eq (get x 'mother) (get y 'mother))   T)
        ((eq (get x 'father) (get y 'father))   T)
        (   T                                   NIL)
    )
)

;----------------------------------------------------------
(set-parents 'I 'A 'B)
(set-parents 'G 'A 'B)
(set-parents 'K 'C 'D)

(print (sisters-brothers 'I 'G))
(print (sisters-brothers 'I 'K))


; 45. Предположим, что у имени города есть свойства х и у, которые содержат 
;координаты места нахождения города относительно некоторого начала 
;координат.Напишите функцию(РАССТОЯНИЕ a b), вычисляющую расстояние между городами а и b.


(defun set-city(lst number1 number2)
    (setf (get lst 'x) number1)
    (setf (get lst 'y) number2)
)


(defun range-bitween-cities (first second)
    ((lambda (xfs yfs)
         (sqrt (+ (* xfs xfs)(* yfs yfs)))
    )(- (get first 'x) (get second 'x))(- (get first 'y)(get second 'y)))
)



;-----------------------------------------------------------------------
(print '(Задача № 45))
(set-city 'SIMF '1.0002 '1.0003)
(set-city 'sevast 1 2)

(print (range-bitween-cities 'simf 'sevast))
