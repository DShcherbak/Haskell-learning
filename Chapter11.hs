module Chapter11 where
import Data.Char
import Chapter9 (myBreak)

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f Leaf         = Leaf
mapTree f (Node l v r) = Node (mapTree f l) (f v) (mapTree f r) 

tree1 :: (Ord a, Num a) => BinaryTree a
tree1 = insert' 5 $ insert' 3 $ insert' 4 $ insert' 1 $ insert' 0 $ insert' 2 $ Leaf

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l v r) = v : (preorder l ++ preorder r)
inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l v r) = inorder l ++ (v : inorder r)
postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node l v r) = postorder l ++ postorder r ++ [v]
testTree :: BinaryTree Integer
testTree =
    Node 
        (Node Leaf 1 Leaf)
    2
        (Node Leaf 3 Leaf)
testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."
testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears"
main :: IO ()
main = do
    testPreorder
    testInorder
    testPostorder

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f def Leaf = def
foldTree f def (Node l v r) = (f v (foldTree f def r)) 

foldTree' :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldTree' f def Leaf = def
foldTree' f def (Node l v r) = (f v (foldTree' f def l) (foldTree' f def r)) 


{- From Chapter 9
caesar :: Int -> [Char] -> [Char]
caesar n [] = []
caesar n (x:xs) = move n x : caesar n xs where 
    move 0 x = x
    move n x | n > 0     = move (n-1) (succ' x)
                | otherwise = move (n+1) (pred' x)
    succ' 'z' = 'a'
    succ' x = if isAlpha x then succ x else x
    pred' 'a' = 'z'
    pred' x = if isAlpha x then pred x else x
-}

type Message = [Char]
type Keyword = [Char]
vigenere :: Keyword -> Message -> Message
vigenere key mes = go key key mes where
    go key save [] = []
    go [] save mes = go save save mes
    go (k:key) save (m:mes) | isAlpha m = upN (fromEnum k - fromEnum 'a') m : go key save mes
                            | otherwise = m : go (k:key) save (mes)

caesar' :: Int -> [Char] -> [Char]
caesar' n ls = go (mod n 26) ls where
    go n [] = []
    go n (x:xs) = (if isAlpha x then move n x else x) : go n xs
    move 0 x = x
    move n x = move (n-1) (if x == 'z' then 'a' else succ x)

class (Enum a, Ord a) => Encircled a where
    upperBound :: a
    lowerBound :: a
    up :: a -> a
    down :: a -> a
    upN :: Int -> a -> a
    downN :: Int -> a -> a


    up x = if x == upperBound then lowerBound else (succ x)
    down x = if x == lowerBound then upperBound else (pred x)
    
    upN n x   = if n < 0 then downN (-n) x else iterate up x !! n
    downN n x = if n < 0 then upN (-n) x else iterate down x !! n 
    

instance Encircled Char where
    upperBound = 'z'
    lowerBound = 'a'

caesar'' :: Int -> [Char] -> [Char]
caesar'' n ls = go (mod n 26) ls where
    go n [] = []
    go n (x:xs) = (if isAlpha x then (upN n x) else x) : go n xs


isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] str = True
isSubseqOf sub [] = False
isSubseqOf sub@(x:xs) (s:str) = if x == s 
                                then isSubseqOf xs str
                                else isSubseqOf sub str  

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (l:ls) = if isAlpha l then toUpper l : ls else l: capitalizeWord ls

capitalizeWords :: String -> [(String, String)]
capitalizeWords str = go (words str) where
    go [] = []
    go (word:xs) = (word, capitalizeWord word) : go xs 
    --go (word@(l:ls):xs) = (word, toUpper l : ls) : go xs

capitalizeSentences :: String -> String
capitalizeSentences = concatMap (\y -> capitalizeWord y ++ ".") . myBreak '.'


data DaPhone = DaPhone [(Char, [Char])] deriving (Show)

convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "OK. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

type Digit = Char
-- Valid presses: 1 and up
type Presses = Int

myPhone = DaPhone [('1', "1"),
                    ('2',"abc2"),
                    ('3',"def3"),
                    ('4',"ghi"),
                    ('5',"jkl5"),
                    ('6',"mno6"),
                    ('7',"pqrs7"),
                    ('8',"tuv8"),
                    ('9',"wxyz9"),
                    ('*',"^*"),
                    ('0'," +0"),
                    ('#',".,#")]

-- | 1 | 2 ABC | 3 DEF |
-----------------------------------------
-- | 4 GHI | 5 JKL | 6 MNO |
-----------------------------------------
-- | 7 PQRS | 8 TUV | 9 WXYZ |
-----------------------------------------
-- | * ^ | 0 + _ | # ., |
-----------------------------------------

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps dph c = if isUpper c then ('*', 1) : res else res where  
    res = rev' dph (toLower c)
    rev' (DaPhone []) c = [] 
    rev' (DaPhone ((d,w):ws)) c = if c `elem` w 
                                    then (d, rev'' c w 1):[]
                                    else rev' (DaPhone ws) c 
    rev'' c [] n = n
    rev'' c (w:ws) n = if w == c then n else rev'' c ws (n+1) 

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead ph = concatMap (reverseTaps ph) 

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr ((+) . snd) 0 

data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x)       = x
eval (Add ex1 ex2) = eval ex1 + eval ex2  

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add ex1 ex2) = printExpr ex1 ++ " + " ++ printExpr ex2



data Price =
    Price Integer deriving (Eq, Show)

data Manufacturer = 
    Mini | Mazda | Tata deriving (Eq, Show)

data Airline = 
    PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = 
    Car Manufacturer Price | Plane Airline deriving (Eq, Show)

myCar :: Vehicle
myCar = Car Mini (Price 14000)
urCar :: Vehicle
urCar = Car Mazda (Price 20000)
clownCar :: Vehicle
clownCar = Car Tata (Price 7000)
doge :: Vehicle
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _       = False
isPlane :: Vehicle -> Bool
isPlane (Plane _) = True 
isPlane _         = False
areCars :: [Vehicle] -> [Bool]
areCars = map isCar 

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car manu _) = Just manu
getManu (Plane _) = Nothing

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show)

instance TooMany Goats where
    tooMany (Goats n) = n > 43

instance TooMany (Int, String) where
    tooMany (n,str) = n > 43

instance TooMany (Int, Int) where
    tooMany (n,m) = (n + m) > 43

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (n,m) = tooMany n && tooMany m

data Person =
    Person { 
        name :: String , 
        age :: Int } 
    deriving (Eq, Show)


type AuthorName = String
data Author =   
    Fiction AuthorName
    | Nonfiction AuthorName
    deriving (Eq, Show)

data GuessWhat =
    ChickenButt deriving (Eq, Show)
data Id a =
    MkId a deriving (Eq, Show)
data Product a b =
    Product a b deriving (Eq, Show)
data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)
data RecordProduct a b =
    RecordProduct { 
        pfirst :: a, 
        psecond :: b 
    } deriving (Eq, Show)

data OperatingSystem =
                GnuPlusLinux
                | OpenBSDPlusNevermindJustBSDStill
                | Mac
                | Windows
                deriving (Eq, Show)
data ProgLang =
                Haskell
                | Agda
                | Idris
                | PureScript
                deriving (Eq, Show)

data Programmer =
        Programmer { 
            os :: OperatingSystem, 
            lang :: ProgLang 
        } deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux,
                        OpenBSDPlusNevermindJustBSDStill, 
                        Mac, 
                        Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]