module N1_3 where

import Nat
power :: Nat -> Nat -> Nat
power a Zero = Succ Zero
power a (Succ b) = a * (power a b)