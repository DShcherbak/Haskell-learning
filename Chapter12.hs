module Chapter12 where

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

replaceThe :: String -> String
replaceThe = unwords . map (fromMaybe "a". notThe) . words

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = helper 0 . map notThe . words
    where helper n [] = n
          helper n [x] = n
          helper n (Nothing : Just x : xs) = if vowelFirst x then helper (n+1) xs
                                                             else helper n xs
          helper n (x : xs) = helper n xs
          vowelFirst [] = False
          vowelFirst (x:_) = x `elem` "aoiue"


countVowels :: Num t => [Char] -> t
countVowels xs = helper 0 xs where 
    helper n [] = n 
    helper n (x:xs) = helper (n + if x `elem` "aoiue" then 1 else 0) xs

newtype Word' = Word' String deriving (Eq, Show)
vowels :: [Char]
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord xs = if length xs < (2 * countVowels xs) then Nothing else Just $ Word' xs

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero    = 0 
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x | x < 0     = Nothing
               | x == 0    = Just Zero 
               | otherwise = Just $ Succ $ fromMaybe Zero $ integerToNat (x-1)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust 

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just x) = f x 
mayybee b _ _ = b 
 

fromMaybe :: a -> Maybe a -> a 
fromMaybe _ (Just x) = x 
fromMaybe a _  = a 


listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing 
listToMaybe (x:xs) = Just x  

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList _ = []

catMaybe :: [Maybe a] -> [a]
catMaybe [] = [] 
catMaybe ((Just x) : xs) = x : catMaybe xs 
catMaybe (_ : xs) = catMaybe xs 

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just [] 
flipMaybe (Nothing : _) = Nothing  
flipMaybe (Just x : xs) = case flipMaybe xs of 
    Nothing -> Nothing 
    Just ys -> Just (x : ys) 

lefts' :: [Either a b] -> [a] 
lefts' = foldr (\x y -> case x of Right _ -> y; Left a -> a:y) [] 


rights' :: [Either a b] -> [b] 
rights' = foldr (\x y -> case x of Right b -> b:y; Left _ -> y) [] 

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr (\eit (as, bs) -> case eit of Left a -> (a:as, bs)
                                                        Right b -> (as, b:bs)) ([], [])


eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing 
eitherMaybe' f (Right b) = Just $ f b  
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a 
either' _ g (Right b) = g b 
eitherMaybe'' ::       (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' toNothing (Just . f) where 
    toNothing _ = Nothing 

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' f x = case f x of 
    Nothing -> []
    Just (a,b) -> a : unfoldr' f b  


betterIterate :: (a -> a) -> a -> [a]
betterIterate f = unfoldr' (\t -> Just (t, f t))   

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)



unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f gen = case f gen of 
    Nothing -> Leaf 
    Just (a,b,c) -> Node (unfold f a) b (unfold f c)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\m -> if m >= n then Nothing else Just (m+1, m, m+1)) 0

