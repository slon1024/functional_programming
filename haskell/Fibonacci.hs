module Fibonacci where

fibonacci :: Integer -> Integer
fibonacci = fib0

{-
- The same code in "different ways"
-}
fib0 0 = 0
fib0 1 = 1
fib0 n = fib0(n-1) + fib0(n-2)

fib1 n | n == 0  = 0
       | n == 1  = 1
       | n > 1   = fib1(n-1) + fib1(n-2)


fib2 n = case n of
            0 -> 0
            1 -> 1
            _ -> fib2(n-1) + fib2(n-2)

fib3 n = 
    if n == 0 
        then 0 
        else if n == 1 
            then 1 
            else fib3(n-1) + fib3(n-2)

{-
- More optimized version
-}
fib4 n | 1 == n = 1
       | n > 1  = fib4' n 2 1 1
       where
           fib4' :: Integer -> Integer -> Integer -> Integer -> Integer
           fib4' n k f_2 f_1 | k == n   = f_1
                             | k < n    = fib4' n (k+1) f_1 (f_1 + f_2)
               
fib5   = 0 : fib5''
fib5'  = zipWith (+) fib5 fib5''
fib5'' = 1 : fib5' 