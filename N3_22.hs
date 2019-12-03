module N3_22 where

import Nat
import Prelude 

data Stream a = Cons a (Stream a) 
    deriving (Eq, Ord)

infixr 5 `Cons`


instance Show a => Show (Stream a) where
  showsPrec p (Cons x xs) = 
    showParen (p > consPrecedence)   $
    showsPrec (consPrecedence + 1) x .
    showString " <:> "               .
    showsPrec consPrecedence xs
      where
        consPrecedence = 5 :: Int

headd  ::  Stream  a  ->  a 
headd  (Cons  x  _  )  =  x 

taill :: Stream a -> Stream a
taill (Cons _ xs) = xs

(!!!) :: Stream a -> Int -> a
(!!!) (Cons x xs) n
  | n == 0    = x
  | n > 0     = (!!!) (xs) (n - 1)
  | otherwise = error "negative argument"

takee :: Int -> Stream a  -> [a]
takee n ~(Cons x xs)
  | n == 0    = []
  | n > 0     =  x : (takee (n - 1) xs)
  | otherwise = error "negative argument"

mapp :: (a -> b) -> Stream a -> Stream b
mapp f ~(Cons x xs) = Cons (f x) (mapp f xs)

filterr :: (a -> Bool) -> Stream a -> Stream a
filterr p ~(Cons x xs)
  | p x       = Cons x (filterr p xs)
  | otherwise = filterr p xs

zipp :: Stream a -> Stream b -> Stream (a,b)
zipp ~(Cons x xs) ~(Cons y ys) = Cons (x,y) (zipp xs ys)

zipWithh :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithh f ~(Cons x xs) ~(Cons y ys) = Cons (f x y) (zipWithh f xs ys)

iteratee :: (a -> a) -> a -> Stream a
iteratee f x = Cons x (iteratee f (f x))

nats = iteratee Succ Zero
constStream 5 = iteratee (\x -> x) 5