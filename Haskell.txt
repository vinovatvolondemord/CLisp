```haskell
--1.Определите функцию, возвращающую последний элемент списка.

accumSum :: [Integer] -> Integer
accumSum  l =  if tail l == []
                  then head l
                  else let xs = tail l  
                       in  accumSum  xs        
main = print ( accumSum [1..6])
```
