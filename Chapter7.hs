{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
module Chapter7 where

avgGrade :: (Fractional a, Ord a) => a -> Char

avgGrade x
    | otherwise = 'E'
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
        where y = x / 100


fiveLast :: [a] -> [a]
fiveLast = take 5 . reverse

f :: [Char] -> Int
f = length . filter (== 'a')

tensDigit :: Integral a => a -> a
tensDigit x = d
    where d = (mod 10 . fst . divMod x) 10


foldBool :: a -> a -> Bool -> a
foldBool f s takeFirst | takeFirst = f
                       | otherwise = s

foldBool' :: a -> a -> Bool -> a
foldBool' f s takeFirst = if takeFirst then f else s

-- foldBool' :: a -> a -> Bool -> a
-- foldBool' f s takeFirst = case takeFirst of
--     True -> f
--     False -> s

g :: (a -> b) -> (a, c) -> (b, c)
g f (a,c) = (f a, c)


roundTrip :: (Show a, Read b) => a -> b
roundTrip = read .show

main :: IO ()
main = do
print (roundTrip 4 :: Int)
print (id 4)