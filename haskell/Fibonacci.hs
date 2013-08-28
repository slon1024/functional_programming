module Fibonacci where

fibonacci :: Integer -> Integer
fibonacci = fib2

fib0 1 = 1
fib0 2 = 1
fib0 n = fib0(n-2) + fib0(n-1)

fib1 n | n == 1    = 1
       | n == 2    = 1
       | otherwise = fib1(n-2) + fib1(n-1)

fib2 n | 1 == n = 1
       | n > 1  = fib2' n 2 1 1
       where
           fib2' :: Integer -> Integer -> Integer -> Integer -> Integer
           fib2' n k f_2 f_1 | k == n   = f_1
                             | k < n    = fib2' n (k+1) f_1 (f_1 + f_2)
                
