module Chapter9 where
import Data.Char

accronym :: String -> String
accronym str = [x | x <- str, x `elem` ['A'..'Z']]

myWords :: String -> [String]
myWords [] = []
myWords word  = filter (/= "") $ takeWhile (/= ' ') word : myWords (dropWhile (== ' ') . dropWhile (/= ' ') $ word)

myWords' :: String -> [String]
myWords' [] = []
myWords' (' ':xs) = myWords $ dropWhile (== ' ') xs
myWords' word  = takeWhile (/= ' ') word : myWords (dropWhile (/= ' ') word)

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines [] = []
myLines ('\n':xs) = myLines $ dropWhile (== '\n') xs
myLines word  = takeWhile (/= '\n') word : myLines (dropWhile (/= '\n') word)

myBreak :: Char -> String -> [String]
myBreak _ [] = []
myBreak br (x:xs) | br == x  = myBreak x $ dropWhile (== '\n') xs
                  | otherwise = takeWhile (/= br) (x:xs) : myBreak br (dropWhile (/= br) xs)

myWords'' :: String -> [String]
myWords'' = myBreak ' '
myLines'' :: String -> [String]
myLines'' = myBreak '\n'

shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]

main :: IO ()
main =
    print $
    "Are they equal? "
    ++ show (myLines sentences
    == shouldEqual)

eftBool :: Bool -> Bool -> [Bool]
eftBool st fin | st > fin = []
               | st == fin = [fin]
               | otherwise = st : eftBool (succ st) fin
eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd st fin | st > fin = []
              | st == fin = [fin]
              | otherwise = st : eftOrd (succ st) fin
eftInt :: Int -> Int -> [Int]
eftInt st fin | st > fin = []
              | st == fin = [fin]
              | otherwise = st : eftInt (succ st) fin
eftChar :: Char -> Char -> String
eftChar st fin | st > fin = []
               | st == fin = [fin]
               | otherwise = st : eftChar (succ st) fin

mySqr :: [Integer]
mySqr = [x^2 | x <- [1..10]]

ex1 :: [Integer]
ex1 = [x | x <- mySqr, even x]

ex2 :: [(Integer, Integer)]
ex2 = [(x, y) | x <- mySqr, x < 50, y <- mySqr, y > 50]

ex3 :: [(Integer, Integer)]
ex3 = take 5 ex2

lx11 = [x^y | x <- [1..5], y <- [2, undefined]]
ly12 = take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
lx13 = sum [1, undefined, 3]
ly14 = length [1, 2, undefined]
lx15 = length $ [1, 2, 3] ++ undefined
ly16 = take 1 $ filter even [1, 2, 3, undefined]
lx17 =take 1 $ filter even [1, 3, undefined]
ly18 = take 1 $ filter odd [1, 3, undefined]
ly19 = take 2 $ filter odd [1, 3, undefined]
lx20 = take 3 $ filter odd [1, 3, undefined]

lx21 = take 1 $ map (+1) [undefined, 2, 3]
ly22 = take 1 $ map (+1) [1, undefined, 3]
lx23 = take 2 $ map (+1) [1, undefined, 3]

itIsMystery = map ((\x -> if x then 'X' else '_') . (`elem` "aeiouy"))

f1 = map (^2) [1..10]
f2 = map minimum [[1..10], [10..20], [20..30]]
-- n.b. minimum is not the same function
-- as the min function that we used before
f3 = map sum [[1..5], [1..5], [1..5]]

myZip :: [a] -> [b] -> [(a, b)]
myZip [] xs = []
myZip xs [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZipWith :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
myZipWith _ [] xs         = []
myZipWith _ xs []         = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)

filterUpper :: [Char] -> [Char]
filterUpper = filter isUpper
makeUpper :: [Char] -> [Char]
makeUpper = map toUpper

bigHead :: [Char] -> Char
bigHead = toUpper . head 

capitalize :: [Char] -> [Char]
capitalize x = bigHead x : tail x 


caesar :: Int -> [Char] -> [Char]
caesar n [] = []
caesar n (x:xs) = move n x : caesar n xs
        where 
            move 0 x = x
            move n x | n > 0     = move (n-1) (succ' x)
                     | otherwise = move (n+1) (pred' x)
            succ' 'z' = 'a'
            succ' x = if isAlpha x then succ x else x
            pred' 'a' = 'z'
            pred' x = if isAlpha x then pred x else x

myConcatMap :: (t -> [a]) -> [t] -> [a]
myConcatMap _ [] = []
myConcatMap f (x:xs) = f x ++ myConcatMap f xs

myConcat :: [[a]] -> [a]
myConcat = myConcatMap id

myMaxBy :: (a -> a -> Ordering) -> [a] -> a
myMaxBy f [] = error "Empty list"
myMaxBy f (x:xs) = go x f xs where
    go x f [] = x
    go x f (y:ys) = if y `f` x == GT then go y f ys else go x f ys

myMax :: (Ord a) => [a] -> a
myMax = myMaxBy compare
             