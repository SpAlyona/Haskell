module Nat where
 
--import Prelude()
data Nat = Zero | Succ Nat | Pred Nat
    deriving (Show, Eq, Ord)

mypred :: Nat -> Nat
mypred Zero = error "Undefined"
mypred (Succ x) = x

_sum :: Nat -> Nat -> Nat
_sum a Zero = a
_sum (Succ a) (Succ b) = if a > b
  then Succ(Succ a) + b
  else a + Succ(Succ b)
 
instance Num Nat where
    (+) a Zero     = a
    (+) a (Succ b) = Succ (a + b)
    (+) a (Pred b) = Pred (b - a)
 
    (*) a Zero     = Zero
    (*) a (Succ b) = a + (a * b)
    (*) a (Pred b) = b - (b * a)

    
 
    fromInteger 0  = Zero
    fromInteger n  = Succ (fromInteger (n-1))
 
    abs x          = x
    signum Zero    = Zero
    signum _       = Succ Zero
 
    negate _       = error "negate is undefined for Nat"