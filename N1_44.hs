module N1_44 ( BinaryTree(Node, EmptyT), reverseTree, right, left ) where

data BinaryTree a = EmptyT | Node a (BinaryTree a) (BinaryTree a)
     deriving (Show, Eq)

left :: BinaryTree a -> BinaryTree a
left EmptyT = EmptyT
left (Node _ l _) = l

right :: BinaryTree a -> BinaryTree a
right EmptyT = EmptyT
right (Node _ _ r) = r

leaves :: BinaryTree a -> [a]
leaves EmptyT = []
leaves (Node a l r) = x ++ y ++ z
  where
    x = leaves l
    y = [a]
    z = leaves r


maxDepth :: BinaryTree a -> Int
maxDepth EmptyT = 0
maxDepth (Node _ left right) = 1 + (max (maxDepth left) (maxDepth right))

reverseTree :: BinaryTree a -> BinaryTree a
reverseTree EmptyT = EmptyT
reverseTree (Node nodeValue leftTree rightTree) =
  (Node nodeValue (reverseTree rightTree) (reverseTree leftTree))

basicTree :: BinaryTree Int
basicTree = (Node 8
              (Node 5
                (Node 9
                  EmptyT
                  EmptyT)
                (Node 7
                  (Node 1 EmptyT EmptyT)
                  (Node 12
                    (Node 2 EmptyT EmptyT)
                    EmptyT)))
             (Node 4
               EmptyT
               (Node 11
                 (Node 3 EmptyT EmptyT)
                 EmptyT)))