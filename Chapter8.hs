module Chapter8 where

import Data.List (intersperse, intercalate)

recurSum :: (Ord a, Num a) => a -> a
recurSum n | n < 1 = error "Only positive input available"
           | n == 1 = 1
           | otherwise = n + recurSum (n-1)

recurMultiply :: (Integral a) => a -> a -> a
recurMultiply 0 x = 0
recurMultiply x 0 = 0
recurMultiply 1 x = x
recurMultiply x 1 = x
recurMultiply x y = x + recurMultiply x (y-1)

data DividedResult = DividedByZero | Result Integer
div' :: (Ord t, Num t) => t -> t -> DividedResult
div' x y | x < 0 && y < 0 = Result $ go (-x) (-y) 0
         | x < 0 || y < 0 = Result $ negate $ go (abs x) (abs y) 0
         | y == 0         = DividedByZero
         | otherwise      = Result $ go x y 0 where
            go n d c | n < d = c
                     | otherwise = go (n-d) d (c+1)


mc91 :: (Ord b, Num b) => b -> b
mc91 n | n > 100   = n - 10
       | otherwise = mc91 . mc91 $ n + 11


digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord 0 = "zero"
digitToWord _ = "minus"

digits :: Int -> [Int]
digits n | n < 0     = -1 : digits (negate n)
         | n < 10    = [n]
         | otherwise = let (x,y) = divMod n 10 in y : digits x

wordNumber :: Int -> String
--wordNumber = tail . foldl (\x y -> x ++ " " ++ digitToWord y) "" . reverse . digits
wordNumber = intercalate "-" . map digitToWord . reverse . digits


catty :: [Char] -> [Char] -> [Char]
catty x y = x ++ " meow " ++ y

flippy :: [Char] -> [Char] -> [Char]
flippy = flip catty

appedCatty :: [Char] -> [Char]
appedCatty = catty "woops"

frappe :: [Char] -> [Char]
frappe = flippy "haha"

tests :: [Bool]
tests = [appedCatty "woohoo" == "woops meow woohoo",
        frappe "1" == "1 meow haha",
        frappe (appedCatty "2") == "woops meow 2 meow haha",
        appedCatty (frappe "blue") == "woops meow blue meow haha",
        catty (frappe "pink") (catty "green" (appedCatty "blue")) == "pink meow haha meow green meow woops meow blue",
        catty (flippy "Pugs" "are") "awesome" == "are meow Pugs meow awesome"]

--dividedBy 15 2 =
--go 15 2 0
--go 13 2 1
--go 11 2 2
--go 9 2 3
--go 7 2 4
--go 5 2 5
--go 3 2 6
--go 1 2 7
--(7,1)


