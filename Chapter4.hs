module Chapter4 where

data Mood = WooHoo | Take deriving Show

changeMood :: Mood -> Mood 
changeMood Take = WooHoo
changeMood _    = Take

allThem = [1,2]

isPalindome :: Eq a => [a] -> Bool
isPalindome x = x == reverse x

myAbs :: (Ord a, Num a) => a -> a
myAbs x = if x < 0 then negate x else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

xt :: Int -> Int -> Int
xt = (+)

f1 :: Foldable t => t a -> Int
f1 xs = w `xt` 1
    where w = length xs

id1 :: p -> p
id1 = \x -> x

fst' :: (a, b) -> a
fst' (a, b) = a

last3Symbols x = reverse (take 3 (reverse x))

