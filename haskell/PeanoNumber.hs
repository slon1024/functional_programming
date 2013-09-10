module PeanoNumber where

data Peano = Zero | Succ Peano
    deriving (Eq, Show, Ord)

instance Num Peano where
	(+) a Zero     = a
	(+) a (Succ b) = Succ (a + b)
	negate _       = error "negate is undefined for Peano"
	(*) a Zero     = Zero
	(*) a (Succ b) = a + (a * b)
	abs a          = a
	signum Zero    = Zero
	signum _       = Succ Zero
	fromInteger 0  = Zero
	fromInteger n  = Succ ( fromInteger (n-1))
