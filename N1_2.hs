module N1_2 where

import Nat

beside2 :: Nat -> Nat -> Bool 
beside2 a b = Succ a == mypred b || Succ b == mypred a

beside3 :: Nat -> Nat -> Nat -> Bool
beside3 a b z = (b == a + z) || (a == b + z )