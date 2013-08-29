module Prime where

prime :: [Integer]
prime = prime0

prime0 = [ x | x <- [2..], isPrime x] where 
             isPrime x = null [ n | n <- [2..(floor . sqrt . fromIntegral $ x)], mod x n == 0]

prime1 = sieve [2..] where
             sieve (x: xs) = x : sieve [ k | k <- xs, mod k x /= 0]

