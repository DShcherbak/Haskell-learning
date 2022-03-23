{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use and" #-}
{-# HLINT ignore "Use or" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use concat" #-}
module Chapter10 where

import Data.Time
data DatabaseItem = DbString String
                | DbNumber Integer
                | DbDate UTCTime  deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase =
            [   DbDate (UTCTime
                            (fromGregorian 1911 5 1)
                            (secondsToDiffTime 34123)
                        ),
                DbNumber 9001,
                DbNumber 307,
                DbString "Hello, world!",
                DbDate (UTCTime
                            (fromGregorian 1921 5 1)
                            (secondsToDiffTime 34123))
            ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate ((DbDate x):xs) = x : filterDbDate xs
filterDbDate (x:xs) = filterDbDate xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber ((DbNumber x):xs) = x : filterDbNumber xs
filterDbNumber (x:xs) = filterDbNumber xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb = uncurry (/) . foldr ((\x (a,b) -> (x+a,b+1)) . fromInteger) (0,0) . filterDbNumber

--fol1 = foldr (*) 1 [1..5]
fol1 :: Integer
fol1 = product [1..5]

--foldl (flip (*)) 1 [1..3]
-- Let <*> = flip (*)

--foldl <*> 1 [1,2,3]
--foldl <*> (1 <*> 1) [2,3]
--foldl <*> ((1 <*> 1) <*> 2) [3]
--foldl <*> (((1 <*> 1) <*> 2) <*> 3) []
--(((1 <*> 1) <*> 2) <*> 3) 
--(((1 * 1) <*> 2) <*> 3) 
--((1 <*> 2) <*> 3) 
--((2 * 1) <*> 3) 
--(2 <*> 3) 
--(3 * 2) 
--(6) 
pab :: [[Char]]
pab = ["Apple", "Banana", "Pizza"]

first3Left :: [[Char]] -> [Char]
first3Left = foldl (\acc elem -> acc ++ take 3 elem) ""

first3Right :: [[Char]] -> [Char]
first3Right = foldr (\elem acc -> take 3 elem ++ acc) ""

first3Left' :: [[Char]] -> [Char]
first3Left' = foldl (\acc elem -> take 3 elem ++ acc) ""

first3Right' :: [[Char]] -> [Char]
first3Right' = foldr (\elem acc -> acc ++ take 3 elem) ""

fibs = 1 : scanl (+) 1 fibs

scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f q ls = q : (case ls of
    [] -> []
    x:xs -> scanl' f (f q x) xs)

-- fibs = 1 : scanl (+) 1 fibs
-- fibs = 1 : (scanl (+) 1 (1:rest))
-- fibs = 1 : 1 : scanl (+) (1 + 1) (1:rest')
-- fibs = 1 : 1 : scanl (+) (2) (1:rest')
-- fibs = 1 : 1 : 2 : scanl (+) (2 + 1) (2:rest'')
-- fibs = 1 : 1 : 2 : scanl (+) (3) (2:rest'')
-- fibs = 1 : 1 : 2 : 3 : scanl (+) (3 + 2) (3:rest'')
-- fibs = 1 : 1 : 2 : 3 : scanl (+) (5) (3:rest''')
-- fibs = 1 : 1 : 2 : 3 : 5 : scanl (+) (5 + 3) (5:rest4)
-- fibs = 1 : 1 : 2 : 3 : 5 : scanl (+) (8) (5:rest4)
-- fibs = 1 : 1 : 2 : 3 : 5 : 8 : scanl (+) (8 + 5) (5:rest4)


stops :: [Char]
stops = "pbtdkg"
vowels :: [Char]
vowels = "aeiou"

pseudoWords :: [[Char]]
pseudoWords = [[x, y, z] | x <- "stops", y <- vowels, z <- stops]
pseudoWords' :: [[Char]]
pseudoWords' = [[x, y, z] | x <- "p", y <- vowels, z <- stops]

nouns :: [[Char]]
nouns = ["man", "tree", "dog", "car"]
verbs :: [[Char]]
verbs = ["own", "buy", "scare"]

pseudoSentences :: [[Char]]
pseudoSentences = [x ++ y ++ z | x <- nouns, y <- verbs, z <- nouns]

avgWordLength :: String -> Double
avgWordLength str = x / y where
    x = fromIntegral (sum (map length (words str)))
    y = fromIntegral (length (words str))

foldAnd :: (Foldable t) => t Bool -> Bool 
foldAnd = foldr (&&) True 

foldOr :: (Foldable t) => t Bool -> Bool 
foldOr = foldr (||) False

foldAny :: (a -> Bool) -> [a] -> Bool
foldAny pred = foldr ((||) . pred) False

foldElem :: Eq a => a -> [a] -> Bool
foldElem el = foldr ((||) . (==el)) False

foldElem' :: Eq a => a -> [a] -> Bool
foldElem' el = foldAny (==el)

foldReverse :: [a] -> [a]
foldReverse = foldl (flip (:)) []

foldReverse' :: [a] -> [a] -- bad, just to practice foldr
foldReverse' = foldr (\x y -> y ++ [x]) [] 

foldMap' :: (a -> b) -> [a] -> [b]
foldMap' f = foldr (\x y -> f x : y) []

foldFilter :: (a -> Bool) -> [a] -> [a]
foldFilter pred = foldr (\x y -> if pred x then x : y else y) []

foldConcat :: [[a]] -> [a]
foldConcat = foldr (++) []

foldConcatMap :: (a -> [b]) -> [a] -> [b]
foldConcatMap f = foldr (\x y -> f x ++ y) []

concatAgain :: [[b]] -> [b]
concatAgain = foldConcatMap id

myMaxByFold :: (Ord a) => (a -> a -> Ordering) -> [a] -> a
myMaxByFold _ [] = error "Empty list" 
myMaxByFold cmp (t:ts) = foldr (\x y -> if cmp x y == LT then y else x) t ts 

factScan :: Int -> Integer
factScan n = flip (!!) n $ scanl (*) 1 [1..]