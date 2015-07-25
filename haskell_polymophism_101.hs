{-# LANGUAGE UnicodeSyntax #-}
----------------------------------------------------
-- Introduction to Polymorphism                   --
----------------------------------------------------

-- https://github.com/MnO2/slides


module Polymorphism_101 where


-- # Polymorphism
--
-- * It refers to a single "interface" to entities of different "type", you want to express the idea once and be applied to different places


-- Suppose that we have a function "replicate" items.
replicateMono :: Int -> Int -> [Int]
replicateMono o 0 = []
replicateMono o n = o : (replicateMono o (n-1))

-- $ >>> replicateMono 1 3
-- [1,1,1]


-- With the above definition it could only replicate "Int", but we don't like to copy and paste the same idea for different types.
--
-- To make it into polymorphic function, We just need to put "type variable" into the type signature.

replicatePoly :: a -> Int -> [a]
replicatePoly o 0 = []
replicatePoly o n = o : (replicatePoly o (n-1))

-- $ >>> replicatePoly True 3
-- [True,True,True]

-- the "a" could be unicode.
replicatePoly' :: ಠ_ಠ -> Int -> [ಠ_ಠ]
replicatePoly' o 0 = []
replicatePoly' o n = o : (replicatePoly' o (n-1))
-- $ >>> replicatePoly' True 3
-- [True,True,True]


-- Not only you could define polymorphic function, you could also define polymorphic data structure
data List a = Nil
            | Cons a (List a)
            deriving (Show)

-- The "a" could be anything, so it is polymorphic List

-- Then we can define helper function on polymorphic List
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


-- And higher order function to transform polymorphic List
map' :: (a -> b) -> List a -> List b
map' f Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

-- $ >>> map' (\x -> not . even $ x) intList
-- Cons True (Cons False (Cons True Nil))


---------------------------
-- Thanks for listening! --
---------------------------
