module Main where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Monad (when)
import Control.Concurrent (forkIO, threadDelay, takeMVar, MVar, putMVar, newEmptyMVar)
import Data.Foldable (for_)
import Data.List (sort)
import Language.Haskell.TH (safe)

type ThreadChan = Chan (Either Bool ([Int], Bool))
type CustomChan = (ThreadChan, ThreadChan)

data Number1 = Int :+ Int deriving Show
data Number2 = Number1 :* Number1


goodForNothing :: (Number1, ((Char, [Char]), [[Char]], [Char])) -> [Char]
goodForNothing (1 :+ m, (('a', "(b,c)"), ["d,e", _, n, '(' : 'f' : ",g)"], "( asd)" )) = "PARSED!"
goodForNothing _ = "doesn't parse..."

calc (x :+ y :* (a :+ b)) = (x + y) * (a + b)

testFunc :: (Eq a1, Num a1) => [a1] -> a1
testFunc [1,x,_] = 1 + x
testFunc xs = 3
{--}

func1, func2 :: Int->Int
func1 x = 1
func2 y = 2

func3 x y = x + y
func4 = func3 3 4 * 9


data Branch = Branch Int Int deriving Show

fbr :: Branch -> Int
fbr (Branch x y) = x *+* x + y *+* y

br1 :: Branch
br1 = Branch 1 (2 *+* 2)


superHardWhere :: Num a => a -> a
superHardWhere x =let y = z + 1 where z = q - u where  q = w + t where w = e - 1               
                                                                       e = x + 1
                                                       t = 1
                                      uu = 4 --change this to u change the asnwer, 
                                       -- means u from a later let is visible inside of wheres
                      u = 1 in
                           y + o where{o = 1}


superHardWhere2 :: Num a => a -> a
superHardWhere2 x = let{y = let{q = z - 1;g = 4}in q + 1 where{z = q - u where{ q = w + 1 where{w = e - 1;e = x + 1}};uu = 4};u = 1}in y + o where{o = 1}

sumOfTwo :: Num a => a -> a -> a
sumOfTwo a b = a + let x = b in x 

superHardWhere' :: Num a => a -> a
superHardWhere' x = let y = z + 1 where z = q - u where  q = w + t where w = e - 1
                                                                         e = x + 1
                                                         t = 1;
                                        uu = 4
                        u = 1
                        pp = 0 in
                           y + o where o = 1

superHardWhere'' :: Num a => a -> a
superHardWhere'' x = let{y = z + 1 where z = q - u where  q = w + t where w = e - 1
                                                                          e = x + 1
                                                          t = 1
                                         uu = 4;u = 1;pp = 0}in
                           y + o where o = 1

--commment
{- long 
    comment -}

{-
    nested {-
        long 
        comment
    -}
-}

--short comment can't contain a long comment

superHardWhere''' :: Num a => a -> a
superHardWhere''' x = let{y = z + 1 where{z = q - u where{ q = w + t where{w = e - 1;e = x + 1};t = 1};uu = 4};u = 1;pp = 0}in
                           y + o where{o = 1}

superHardWhere'''' :: Num a => a -> a
superHardWhere'''' x = let{y = z + 1 where{z = q - u where{ q = w + t where{w = e - 1;e = x + 1};t = 1};uu = 4};u = 1;pp = 0}in y + o where{o = 1}


superHardLet x = let 
                        y = a + 1; 
                        a = let {p = 1} in p + 1 in y

superHardLet' x = let{;y = a + 1 
                      ;a = let {p = 1} 
                            in p + 1 
                     ;}in y


superHardLet'' x = let{y = let{z = x}in z}in y 





templateExample :: (Int, Int, [Char]) -> Char
templateExample (0, _, _ ) = '0'
templateExample (a, b, []) = if a < b then 'a' else 'b'
templateExample (_, _, x:xs) = x  

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)



infixl 5 *+*
(*+*) :: Num a => a -> a -> a
(*+*) x y = x*x + y*y 

infixl 7 ***
(***) :: Num a => a -> a -> a
(***) x y = x*x + y*y 

addSumSquares x1 y1 x2 y2 = x1 *+* y1 + x2 *+* y2
addSumSquares' x1 y1 x2 y2 = x1 *+* (y1 + x2) *+* y2
addSumSquares'' x1 y1 x2 y2 =(x1 *** y1) + (x2 *** y2)




letletinin x = let z = let y = b + 1; b = x - 1 in y + 1 in z

letinletin x = let y = x + 1 in let z = y + 1 in z




makeNChannels :: Int -> IO [CustomChan]
makeNChannels 0{-xx asax {-nested-}-} = return []
makeNChannels n = do
    rest <- makeNChannels (n-1)
    cur_in <- newChan
    cur_out <- newChan
    return ((cur_in, cur_out):rest)

unsortedArray = [16,15,4,5,13,7,8,11,12,2,1,3,10,6,9]

splitEveryLimit :: Int -> [a] -> Int -> [[a]]
splitEveryLimit _ [] m = []
splitEveryLimit _ xs 0 = [xs]
splitEveryLimit n list m = first : splitEveryLimit n rest (m-1)
  where
    (first,rest) = splitAt n list

subdivideArray :: [a1] -> Int -> [[a1]]
subdivideArray arr m = let n = length arr; p = div n m in
    disposeFirst (reverse (splitEveryLimit p arr m))
        where
            disposeFirst [x] = [x]
            disposeFirst [] = []
            disposeFirst list@(x:xs) = if length list == m then list
                                         else smashFirst x xs
            smashFirst [] xs = xs
            smashFirst xs [] = []
            smashFirst (x:xs) (ys:yys) = (x:ys) : smashFirst xs yys


fuckedUpWhere x y = sum x y where {
    sum x y = x + abs' y 
    ;abs' y = 1 + desc' y 
    ;desc' z = shsh z - 1 
        where{
            shsh x = x }}

mainThread :: Chan (IO [Int]) -> Int -> [CustomChan] -> MVar Bool -> IO ()
mainThread fin n chans ioMutex = do{
    ;let pairs = fb [0..(n-1)]
    ;goThroughPairs 
        pairs  
    ;writeChan fin $ concatMap fst <$> mapM getFinal [0..n-1]
    ;return ()}
        where{
            goThoughPair (up, down) = do{
                ;safePrint ioMutex "GOING IN"
                ;let (to_up, from_up) = chans !! up
                ;let (to_down, from_down) = chans !! down
                ;writeChan to_up (Left True)
                ;writeChan to_down (Left True)
                ;--sleepMs 100
                ;Right (vu,_) <- readChan from_up
                ;Right (vd,_) <- readChan from_down

                ;safePrint ioMutex $ show up ++ " " ++ show vu
                ;safePrint ioMutex $ show down ++ " " ++ show vd

                ;writeChan to_up (Right (vd, True))
                ;writeChan to_down (Right (vu, False))
                ;safePrint ioMutex "End main"} 
            ;getFinal x = do
                let (to_chan, from_chan) = chans !! x
                writeChan to_chan (Left False)
                --sleepMs 100 -- so I don't get my own message
                Right res <- readChan from_chan
                return res
            ;goThroughPairs prs = do
                 for_ [0..length prs-1] go where
                    go i = goThoughPair (prs !! i)}

safePrint :: MVar Bool -> String -> IO()
safePrint ioMutex s = do
    available <- takeMVar ioMutex
    putStrLn s
    putMVar ioMutex available
    return ()


workerThread ::  [Int] -> CustomChan -> MVar Bool -> IO()
workerThread array chans@(chan_in, chan_out) ioMutex = do
    safePrint ioMutex $ "started with " ++ show array
    Left go_on <- readChan chan_in
    safePrint ioMutex $ "Got " ++ show go_on
    writeChan chan_out (Right (array, True))
    when go_on $ do
        --sleepMs 100
        Right (other, am_I_top) <- readChan chan_in
        let n = length array
        let m = length other
        let mixed = mergeArrays array other
        safePrint ioMutex $ show array ++ " ++ " ++ show other ++ " = " ++ show mixed
        if am_I_top then
            workerThread (take n mixed) chans ioMutex
        else
            workerThread (drop m mixed) chans ioMutex


mergeArrays :: Ord a => [a] -> [a] -> [a]
mergeArrays xs [] = xs
mergeArrays [] ys = ys
mergeArrays (x:xs) (y:ys) = if x < y then x : mergeArrays xs (y:ys)
                                     else y : mergeArrays ys (x:xs)


sorterThread ::  [Int] -> CustomChan -> MVar Bool -> IO()
sorterThread array = workerThread (sort array)

sleepMs n = threadDelay (n * 1000)

startWorkingThreads :: Int -> [CustomChan] -> [[Int]] -> MVar Bool -> IO()
startWorkingThreads n channels arrs ioMutex = do
    for_ [0..n-1] (startFork channels arrs) where
        startFork chans ars i = forkIO(
            do
                sorterThread (ars !! i) (chans !! i) ioMutex)

magicChannels :: Int -> IO ()
magicChannels n = do
    ioMutex  <- newEmptyMVar
    putMVar ioMutex True
    channels <- makeNChannels n
    let arrs = subdivideArray unsortedArray n
    finished <- newChan


    startWorkingThreads n channels arrs ioMutex

    forkIO (do
        mainThread finished n channels ioMutex
        return ())

    result <- readChan finished
    unboxed <- result
    print unboxed
    return ()


fs :: [Int] -> [Int] -> [(Int, Int)]
fs [] [] = []
fs up down = let conc = up ++ down; in
        case length conc of
    1 -> []
    2 -> [firstPair conc]
    n ->
        let
            (up_odd, up_even) = splitParity up;
            (down_odd, down_even) = splitParity down
        in
            fs up_odd down_odd ++ fs up_even down_even ++ [firstPair xx | xx <- map (`drop` conc) [1, 3.. n], length xx > 1]


firstPair xs = (head xs, (head.tail) xs)
splitParity xs = par' 1 [] [] xs where
    par' n od ev [] = (reverse od, reverse ev)
    par' n od ev (x:xs) = if odd n then
                            par' (n+1) (x:od) ev xs
                            else
                            par' (n+1) od (x:ev) xs



fb :: [Int] -> [(Int, Int)]
fb [] = []
fb [x] = []
fb xs = let n = length xs `div` 2; (up, down) = splitAt n xs in
    fb up ++ fb down ++ fs up down



main :: IO()
main = do
    print "pre lab 2"