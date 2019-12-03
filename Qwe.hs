module Qwe where
import Nat
import Prelude

calculateDiscount' :: Customer -> Int -> Int

---------------------------------------------------------------------------------------
-- 1. Define: Types of customers
---------------------------------------------------------------------------------------

data Customer a = Vip | Reg a
        deriving (Eq)

-- 2 = BOOK_THRESHOLD
-- 3 = DISCOUNT_PERCENTAGE
---------------------------------------------------------------------------------------
-- 2. Define: Discountable Customers and Discounts
---------------------------------------------------------------------------------------

{-@ inline customerGetsDiscount @-}
customerGetsDiscount :: Customer -> Int -> Bool
customerGetsDiscount c i = c == Vip && i >= 2

{-@ inline discount @-}
discount :: Int -> Int
discount bookCount = (bookCount - 2) * 3

{-@ type Discount i = {v:Int | v == discount i} @-}

---------------------------------------------------------------------------------------
-- 3. Policy: Only compute discounts for discountable customers
---------------------------------------------------------------------------------------

{-@ calculateDiscount' :: c:Customer -> i:{Int | customerGetsDiscount c i} -> Discount i @-}
calculateDiscount' c i = discount i

---------------------------------------------------------------------------------------
-- 4. Implement: Code to compute discounts, if suitable, is accepted
---------------------------------------------------------------------------------------

{-@ calculateDiscount :: Int -> Nat -> Nat @-}
calculateDiscount userId bookCount
  | getsDiscount = calculateDiscount' c bookCount
  | otherwise    = 0
  where
    getsDiscount = customerGetsDiscount c bookCount
    c            = customerType userId

customerType :: Int -> Customer
customerType = undefined
