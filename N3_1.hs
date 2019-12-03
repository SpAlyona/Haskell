module N3_1 where
import Prelude

square_square :: Double -> Double
square_square n = n * n

square_square2 :: Double -> Double
square_square2 n = c*n^2 where c = 0.5

square_rectangle :: Integer -> Integer -> Integer
square_rectangle n m = n * m

square_triangle :: Double -> Double -> Double -> Double
square_triangle a b c = sqrt (p * pa * pb * pc)
   where p = (a + b + c) / 2
         pa = p - a
         pb = p - b
         pc = p - c

square_triangle2 :: Double -> Double -> Double
square_triangle2 a b = c * a * b
   where c = 0.5

square_circle :: Double -> Double
square_circle r = pi*r^2 
   where pi = 3.14


