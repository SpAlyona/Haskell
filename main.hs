module Logic where

import Prelude(Bool(..), Show(..), Eq(..))
import N1_44
true::Bool
true = True

false::Bool
false = False

not:: Bool -> Bool
not True = False
not False = True

and:: Bool->Bool->Bool
and False _ = False
and True x = x

or:: Bool->Bool->Bool
or False x = x
or True _ = True

xor:: Bool->Bool->Bool
xor a b = or(and (not a)b) (and a(not b))

ifThenElse :: Bool -> a -> a -> a
ifThenElse True t _ = t
ifThenElse False _ e = e
