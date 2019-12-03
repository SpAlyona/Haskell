module N3_2 where
import Nat

import Prelude

data Stream a = a :& Stream a
      deriving (Eq, Ord)


instance Show a => Show (Stream a) where
    show xs = showInfinity (show (take_n 5 xs))
        where showInfinity x = P.init x P.++ "..."

-- Первый элемент потока
head_n :: Stream a -> a
head_n (x :& _ ) = x
-- Хвост потока, всё кроме первого элемента
tail_n :: Stream a -> Stream a
tail_n (_ :& xs) = xs

-- n-тый элемент потока
niy :: Stream a -> Int -> a
niy (x :& xs) t
             |t == 0 = x
             |t > 0 = niy (xs)  (t-1)
             |otherwise = error "Negative argument"

-- Берёт из потока несколько первых элементов:
take_n :: Int -> Stream a -> [a]
take_n n ~(x :& xs)
      |n == 0    = []
      |n > 0     =  x : take_n (n - 1) (xs)
      |otherwise = error "Negative argument"


-- Преобразование потока
map_n :: (a -> b) -> Stream a -> Stream b
map_n f ~(x :& xs) = (f x) :& (map_n f xs)
-- Фильтрация потока
filter_n :: (a -> Bool) -> Stream a -> Stream a
filter_n p ~(x :& xs)
  | p x       = x :& (filter_n p xs)
  | otherwise = filter_n p xs

-- zip-ы для потоков:
zip_n :: Stream a -> Stream b -> Stream (a, b)
zip_n ~(x :& xs) ~(y :& ys) = (x,y) :& (zip_n xs ys)

zipWith_n :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith_n f ~(x :& xs) ~(y :& ys) = (f x y) :& (zipWith_n f xs ys)

-- функция генерации потока
iterate_n :: (a -> a) -> a -> Stream a
iterate_n f a = a :& (iterate_n f (f a))
