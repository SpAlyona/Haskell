module N1_1 where

import Nat
beside :: Nat -> Nat -> Bool
beside a b = a == Succ b || b == Succ a
