8.пределите функцию, которая разделит исходный список из целых чисел надва списка: список положительных чисел и список отрицательных чисел.

```

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
