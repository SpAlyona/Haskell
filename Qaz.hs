module Qaz where
import Prelude

fix f = let x = f x 
          in x 

foldr_n :: (a -> b -> b) -> b -> [a] -> b
foldr_n f = fix (\t z xs -> case xs of 
          [] -> z 
          y:ys -> f y (t z ys))

foldl_n :: (a -> b -> a) -> a -> [b] -> a
foldl_n f = fix (\t z xs -> case xs of 
          [] -> z  
          y:ys -> f (t z ys) y)
