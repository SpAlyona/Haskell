module Student where

import Prelude 

--data Stud = Stud FIO Age Date_start Date_end Num_Stu_Card

data Surname = Surname String
data Name = Name String
data Patronymic = Patronymic String
data Age = Age Int
data Date_start = Date_start Year Month Day
data Date_end = Date_end Year Month Day
data Year = Year Int
data Month = Month Int
data Day = Day Int
data Num_Stu_Card = Num_Stu_Card Symbol Numb
data Symbol = Symbol String
data Numb = Numb Int
data FIO = FIO Surname Name Patronymic
data Stud = Stud FIO Age Date_start Date_end Num_Stu_Card

instance Show Name where
    show (Name n) = show n

instance Show Surname where
    show (Surname s) = show s

instance Show Patronymic where
    show (Patronymic p) = show p

instance Show Age where
    show (Age a) = show a 

instance Show FIO where
    show (FIO f i o) = show f ++ " " ++ show i ++ " " ++ show o

instance Show Year where
    show (Year y) = show y

instance Show Month where
    show (Month m) = show m
{-instance Show Month where
    show January = "Jan"
    show February = "Feb"
    show March = "Mar"
    show April ="Apr"
    show May = "May"
    show June = "Jun"
    show July = "Jul"
    show August = "Aug"
    show September = "Sep"
    show October = "Oct"
    show November = "Nov"
    show December = "Dec"-}

instance Show Day where
    show (Day d) = addZero (show d)

instance Show Date_start where
    show (Date_start y m d) = show d ++ " " ++ show m ++ " " ++ show y

instance Show Date_end where
    show (Date_end y m d) = show d ++ " " ++ show m ++ " " ++ show y

instance Show Stud where
    show (Stud a b c d e) = show a



addZero :: String -> String
addZero (a:[]) = '0' : a : []
addZero as = as

--myname :: Name
--myname = Name "Alyona"
daystud :: Stud -> Int
daystud (Stud (FIO (Surname a) (Name b) (Patronymic c)) (Age d) (Date_start (Year e) (Month f) (Day g)) (Date_end (Year h) (Month i) (Day j)) (Num_Stu_Card (Symbol k) (Numb l))) = (j+i*30+h*365)-(g+f*30+e*365)

mystud :: Stud
mystud = Stud (FIO (Surname "Spichak") ( Name "Alyona") (Patronymic "Vitalevna")) (Age (20)) (Date_start (Year (1234)) (Month (1)) (Day (1))) (Date_end (Year (1234)) (Month (1)) (Day (2))) (Num_Stu_Card (Symbol "df") (Numb (234)))

agestud2 :: Stud -> Age
agestud2 (Stud a b c d e) = b



{-name_a :: FIO
name_a = FIO (Surname "Spichak") ( Name "Alyona") (Patronymic "Vitalevna")

name_b :: Age
name_b = Age (20)

name_c :: Date_start
name_c = Date_start (Year 1234) (Month "March") (Day 1)

name_d :: Date_end
name_d = Date_end (Year 1234) (Month "March") (Day 2)

name_e :: Num_Stu_Card
name_e = Num_Stu_Card (Symbol "df") (Numb 234)-}

