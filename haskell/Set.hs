module Set where

type IntSet = (Integer -> Bool)
empty   :: IntSet
empty e = False

{- (e > 1) && (e < 101)
--
-- from2to100 1
-- False
--
-- from2to100 100
-- True
-}
from2to100 e = (e >= 2) && (e <= 100) 

{- The set of odd numbers
-- 
-- odds 39129
-- True
--
-- odds 3290
-- False
-}
odds   :: IntSet
odds e = mod e 2 == 1 

{- Unions
-- Two sets can be "added" together. 
-- The union of A and B, denoted by A ∪ B, 
-- is the set of all things that are members of either A or B.
-}
union           :: IntSet -> IntSet -> IntSet
(union s0 s1) e = (s0 e) || (s1 e)

{- Intersections
-- 
-- A new set can also be constructed by determining which members two sets have "in common". 
-- The intersection of A and B, denoted by A ∩ B, is the set of all things that are members of both A and B. 
--
-- intersec empty empty is empty
-- intersec odds  from2to100 is [3,5..99]
-}
intersec           :: IntSet -> IntSet -> IntSet
(intersec s0 s1) e = (s0 e) && (s1 e)


{- Complements
-- 
-- Two sets can also be "subtracted". 
-- The relative complement of B in A 
-- denoted by A \ B (or A − B), 
-- is the set of all elements that are members of A but not members of B.
--
-- compl odds from2to100 is [1, 101, 103, 105 ...] 
-}
compl           :: IntSet -> IntSet -> IntSet
(compl s0 s1) e = (s0 e) && False == (s1 e)


{- add new element
--
-- addElem 1 empty
-- \e -> (1 == e) || (empty e)
-- \e -> (1 == e) || False
-}
addElem :: Integer -> IntSet -> IntSet
addElem new s e = (new == e) || (s e)


{- remove element
-- 
-- remElem 1 empty
-- \e -> (1 /= e) && (empty e)
-- \e -> (1 /= e) && False
--
-- remElem 1 (addElem 1 empty)
-- \e -> (1 /= e) && (addElem 1 empty)
-- \e -> (1 /= e) && ((1 == e) || (empty e))
-- \e -> (1 /= e) && ((1 == e) || False)
-- 
-}
remElem :: Integer -> IntSet -> IntSet
remElem new s e = (new /= e) && (s e)
