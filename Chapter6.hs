module Chapter6 where
data Trivial = Trivial

instance Eq Trivial where
    Trivial == Trivial = True

data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving (Ord, Show)
-- day of week and numerical day of month
data Date =
    Date DayOfWeek Int deriving Show



instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Weds Weds = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

instance Eq Date where
    (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
        weekday == weekday' && dayOfMonth == dayOfMonth'


data TwoIntegers = Two Integer Integer
data StringOrInt = TisAnInt Int | TisAString String
data Pair a = Pair a a
data Tuple a b = Tuple a b
data Which a = ThisOne a | ThatOne a
data EitherOr a b = Hello a | Goodbye b

instance Eq TwoIntegers where
    (==) (Two x y) (Two x1 y1) = x == x1 && y == y1

instance Eq StringOrInt where
    (==) (TisAnInt x) (TisAnInt y) = x == y
    (==) (TisAString x) (TisAString y) = x == y
    (==) _ _ = False

instance (Eq a) => Eq (Pair a) where
    (==) (Pair x y) (Pair x1 y1) = x == x1 && y == y1

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x1 y1) = x == x1 && y == y1

instance (Eq a) => Eq (Which a) where
    (==) (ThisOne x) (ThisOne y) = x == y
    (==) (ThatOne x) (ThatOne y) = x == y
    (==) _ _                     = False

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello y)     = x == y
    (==) (Goodbye x) (Goodbye y) = x == y
    (==) _ _                     = False


voidFunc :: Int -> ()
voidFunc x = ()

newtype Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson = print

data Mood = Blah | Woot deriving (Eq, Show)
settleDown :: Mood -> Mood
settleDown x = if x == Woot then Blah else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool"
s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"


newtype Rocks =
    Rocks String deriving (Eq, Show)
newtype Yeah =
    Yeah Bool deriving (Eq, Show)
data Papu =
    Papu Rocks Yeah
    deriving (Eq, Show)

--phew = Papu "chases" True 

phew = Papu (Rocks "chases") (Yeah True)

truth = Papu (Rocks "chomskydoz")
    (Yeah True)
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

--comparePapus :: Papu -> Papu -> Bool
--comparePapus p p' = p > p'
-- needs deriving Ord

i :: Num a => a
i = 1

f:: RealFrac a => a
f = 1.0

freud :: Int -> Int
freud x = x

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = y == f x

arith :: Num b
    => (a -> b)
    -> Integer
    -> a
    -> b
arith f int a = f a + fromIntegral int
