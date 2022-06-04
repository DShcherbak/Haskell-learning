module Main where

import Control.Concurrent
import Data.Foldable
import System.Environment
import GHC.Conc

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

odds :: [a] -> [a] -- public static ArrayList<Integer> odds (ArrayList<Integer> x)
odds [] = []
odds [x] = []
odds (x:y:xs) = y : odds xs

evns :: [a] -> [a]
evns [] = []
evns [x] = [x]
evns (x:y:xs) = x : evns xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise =  y : merge (x:xs) ys


batcherMerge :: Ord a => [a] -> [a]
batcherMerge [] = []
batcherMerge [x] = [x]
batcherMerge arr
    | length arr > 2 = merge mergedEvn mergedOdd
    | otherwise = merge [head arr] [last arr]
    where
        mergedEvn = batcherMerge (evns arr)
        mergedOdd = batcherMerge (odds arr)


batcherSort :: Ord a => [a] -> [a]
batcherSort [] = []
batcherSort [x] = [x]
batcherSort arr = leftSorted `par` batcherMerge (leftSorted ++ rightSorted)
    where
        (left,right) = splitList arr
        leftSorted = batcherSort left
        rightSorted = batcherSort right



splitList :: [a] -> ([a], [a])
splitList [] = ([],[])
splitList [x] = ([x],[])
splitList xs = splitAt mid xs
        where
            mid = length xs `div` 2

main :: IO ()
main = do
    args <- getArgs
    --content <- readLines(head(args))
    let content = [100,99..1]
    print(batcherSort content)