module Pascal where

pascal :: [[Integer]]
pascal = iterate pascal' [1] where
            pascal' xs@(x:ys) = x : pascal'' xs ys where
                pascal'' xs []         = xs
                pascal'' (x:xs) (y:ys) = (+) x y : pascal'' xs ys


