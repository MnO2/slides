
----------------------------------------------------
-- Introduction to Polymorphism                   --
----------------------------------------------------

-- https://github.com/MnO2/slides


module Polymorphism_101 where


replicateMono :: Int -> Int -> [Int]
replicateMono o 0 = []
replicateMono o n = o : (replicateMono o (n-1))

-- $ >>> replicateMono 1 3
-- [1,1,1]


-- Polymorphism

replicatePoly :: a -> Int -> [a]
replicatePoly o 0 = []
replicatePoly o n = o : (replicatePoly o (n-1))

-- $ >>> replicatePoly True 3
-- [True,True,True]


data List a = Nil
            | Cons a (List a)
            deriving (Show)

length' :: List a -> Int
length' Nil = 0
length' (Cons x xs) = 1 + (length' xs)

intList :: List Int
intList = Cons 1 (Cons 2 (Cons 3 Nil))

boolList :: List Bool
boolList = Cons True (Cons False (Cons True Nil))

-- $ >>> length' intList
-- 3

-- $ >>> length' boolList
-- 3


map' :: (a -> b) -> List a -> List b
map' f Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

-- $ >>> map' (\x -> not . even $ x) intList
-- Cons True (Cons False (Cons True Nil))



---------------------------
-- Thanks for listening! --
---------------------------
