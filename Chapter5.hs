module Chapter5 where

anonFunc :: Integer -> Bool -> Integer
anonFunc = \int -> \bool -> (if bool then int * int else abs int)

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

test3 :: Floating a => (a, a, a) -> a
test3 (x,y,z) = sqrt(x*x + y*y + z*z)


-- SECTIONING FOR INFIX FUNCTIONS !!!

-- elem :: a -> [a] -> Bool 

isDigit' :: Int -> Bool
isDigit' = (`elem` [0..9])

has6 :: [Int] -> Bool
has6 = elem 6

notId :: a -> a
notId x = x

-- changeId :: a -> b, not possible, as I assumed

first :: a -> a -> a
first x y = x

second :: a -> a -> a 
second x y = y

-- 6 / length [1,2,3] won't work, length :: [f] -> Int, (/) is for Fractional
div6len :: (Fractional b) => [a] -> b
div6len xs = 6 / fromIntegral (length xs)

--Defines as Integer if no flag for NoMonomorphismRestriction
example :: (Num a) => a
example = 1

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing :: [Char]
sing = if (x < y) then
    fstString x else sndString y where 
        x = "Singin"
        y = "Somewhere"


data Woot
data Blah

f :: Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)
g (x,y) = (x, f y)

f1 :: Int -> String
f1 = undefined

g1 :: String -> Char
g1 = undefined

h1 :: Int -> Char
h1 = g1 . f1

data A
data B
data C
q :: A -> B
q = undefined
w :: B -> C
w = undefined
e :: A -> C
e = w . q 


data X
data Y
data Z
xz :: X -> Z
xz = undefined
yz :: Y -> Z
yz = undefined
xform :: (X, Y) -> (Z, Z)
xform (x,y) = (xz x, yz y)


munge :: (x -> y)
    -> (y -> (w, z))
    -> x
    -> w
munge f g v = fst $ g $ f v


main :: IO ()
main = do
    print $ (1 + 2)
    putStrLn $ show 10
    print $ (negate (-1))
    print ((+) 0 blah)
        where blah = negate 1