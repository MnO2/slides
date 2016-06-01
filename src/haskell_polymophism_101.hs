{-# OPTIONS_GHC -w -fno-warn-tabs -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-missing-signatures -fno-warn-unused-matches #-}

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


-- You could also set the constraint for the type variable with type class
-- Such as built-in 'Show'  (Think it like java interface)
--
print' :: Show a => a -> IO ()
print' = putStrLn . show

-- $ >>> print' (3.14159 :: Double)
-- 3.14159


-- Another commonly used is 'Eq', it means the element is comparable with '=='
any' :: Eq a => a -> [a] -> Bool
any' e [] = False
any' e (h:rest) | e == h = True
any' e (h:rest) | e /= h = any' e rest

-- $ >>> any' 1 [2,3,1]
-- True

-- $ >>> any' 0 [2,3,1]
-- False
--

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


-- reduce the [1,2,3] with (+) operator into 6
-- with association to the left  ((1+2) + 3)
-- i.e.  python's 'reduce' and ruby's 'reduce' function
--
--         (+)
--        /  \
--     (+)     3
--     /  \
--    1    2
--
fold_left_associative :: (b -> a -> b) -> b -> List a -> b
fold_left_associative f e (Cons x xs) = fold_left_associative f (f e x) xs
fold_left_associative f e Nil = e

-- $ >>> fold_left_associative (\x y -> x + y) 0 intList
-- 6


-- reduce the [1,2,3] with (+) operator into 6
-- with association to the right (1 + (2+3))
--
--     (+)
--    /   \
--   1     (+)
--         /  \
--        2    3
--
fold_right_associative :: (b -> a -> b) -> b -> List a -> b
fold_right_associative f e (Cons x xs) = f (fold_right_associative f e xs) x
fold_right_associative f e Nil = e

-- $ >>> fold_right_associative (\x y -> x + y) 0 intList
-- 6


-- We could redefine the any' with fold
any'' :: Eq a => a -> [a] -> Bool
any'' e l = foldl (\z p -> z || p == e) False l

-- $ >>> any'' 1 [2,3,1]
-- True

-- $ >>> any'' 0 [2,3,1]
-- False


-- To show some magic, there is a build-in typeclass callsed 'Foldable', to denote the 'container' that could be fold upon
anyFoldable :: (Foldable t, Eq a) => a -> t a -> Bool
anyFoldable e c = foldl (\z p -> z || p == e) False c

-- $ >>> anyFoldable 1 [2,3,1]
-- True

-- $ >>> anyFoldable 0 [2,3,1]
-- False


---------------------------
-- Thanks for listening! --
---------------------------
