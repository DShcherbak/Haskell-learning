import Debug.Trace(traceShow)

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort left ++ [x] ++ qsort right where
    (left, right) = (filter (<=x) xs, filter (>x) xs)


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

map'' :: Show b => (a -> b) -> [a] -> [b]
map'' _ [] = []
map'' f (x:xs) = traceShow (f x) (f x) : map'' f xs

map''' :: Show b => (a -> b) -> [a] -> [b]
map''' _ [] = []
map''' f (x:xs) = traceShow (f x) (f x : map''' f xs)

st1 :: Num b => [b] -> [b]
st1 = map (+1)