
module N4_1 where

import Prelude hiding (const,(.),id)
import Control.Category 
 
data St a b = St (a -> (b, St a b))
 
ap :: St a b -> [a] -> [b]
ap _ [] = []
ap (St f) (x:xs) = let (x1,f1) = f x in
    x1 : ap f1 xs

instance Category St where
    id = St (\x -> (x, id))
    (.) (St f) (St g) = St (\x -> let {(x1, g1) = g x; (x2,f1) = f x1} in (x2, f1 . g1))
        
const :: a -> St b a
const x = St (\ _ -> (x, const x))
 
integrall :: Num a => St a a
integrall = t (+0) (\x -> ((+x).))
    where t f next_f = St (\x -> (f x, t (next_f x f) next_f))

fix f = let x = f x 
          in x 
func1 = fix (\t -> St(\a -> (a+1,t)))
func2 = fix (\t -> St(\a -> (a*2,t)))

map = fix (\m f xs -> case xs of
   []   -> []
   y:ys -> f y : m f ys)


repeat_n :: a -> [a]
repeat_n a = fix (a:)

iterate_n :: (a -> a)-> a -> [a]
iterate_n f x = fix $ \xs -> x: (f <$> xs)

--foldl_n :: a -> (a -> a) -> (St -> a)
--foldl_n f g a [] = a
--foldl_n f g a (l:ls) = f g (g a l) ls
--foldl_n = fix foldl_n 

foldr_n :: (a -> a -> a) -> [a] -> a
foldr_n f = fix (\t z xs -> case xs of 
          [] -> z 
          y:ys -> f y (foldr_n f z ys))