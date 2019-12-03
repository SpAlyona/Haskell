module Qwer where
import Prelude 

data Human = Human Surname Name Patronymic Age Dolzh
data Surname = Surname String
data Name = Name String
data Patronymic = Patronymic String
data Age = Age Int
data Dolzh = Dolzh String

class H_H a where
  equal :: a -> a -> Bool
  printhuman :: a -> IO ()

instance H_H Human where
  printhuman (Human a) = print a

  equal (Human a) (Human b) 
                          | Age a > Age b = 1
                          | otherwise = 0
                  
myH :: Human
myH = Human (Surname "Spichak") ( Name "Alyona") (Patronymic "Vitalevna") (Age (20)) (Dolzh "df") 