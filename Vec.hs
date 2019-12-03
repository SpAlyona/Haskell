module Vec where

import Prelude (Int, Char, String, Show(..), (++))

data Vector = Vector [Int]

class V_V a where
plus :: a -> a -> Bool
