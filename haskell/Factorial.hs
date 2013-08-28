module Factorial where

fact :: Integer -> Integer
fact = fact0

fact0 0 = 1
fact0 n = n * fact0(n-1)

fact1 n | n == 0 = 1
        | n > 0  = fact1' n 1
        where
            fact1' :: Integer -> Integer -> Integer
            fact1' k v | k == 1 = v
                       | k > 1  = fact1' (k-1) (v*k)

fact2 n = product[1..n]

fact3 n = foldr (*) 1 [1..n]

test = [(fact0 1, fact0 2, fact0 10), (fact3 1, fact3 2, fact3 10)]
