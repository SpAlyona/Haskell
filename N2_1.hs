module N2_1 where
import Prelude


square_square :: Integer -> Integer
square_square n = n * n

square_rectangle :: Integer -> Integer -> Integer
square_rectangle n m = n * m

square_triangle :: Double -> Double -> Double -> Double
square_triangle a b c = sqrt (p * pa * pb * pc)
   where p = (a + b + c) / 2
         pa = p - a
         pb = p - b
         pc = p - c

square_circle :: Double -> Double
square_circle r = pi*r^2 
   where pi = 3.14


