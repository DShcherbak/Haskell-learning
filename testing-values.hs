module Main where

main :: IO()
main = do
    print("Hi")
{-
foldr (\s acc -> ("0"++s) : ("1"++s) : acc) [] $ gray1 (n - 1)
gray 3
foldr (\s acc -> ("0"++s) : ("1"++s) : acc) [] $ gray 2
foldr (\s acc -> ("0"++s) : ("1"++s) : acc) [] $ foldr (\s acc -> ("0"++s) : ("1"++s) : acc) [] $ gray 1
foldr (\s acc -> ("0"++s) : ("1"++s) : acc) [] $ foldr (\s acc -> ("0"++s) : ("1"++s) : acc) [] $ foldr (\s acc -> ("0"++s) : ("1"++s) : acc) [] $ gray 0
foldr (\s acc -> ("0"++s) : ("1"++s) : acc) [] $ foldr (\s acc -> ("0"++s) : ("1"++s) : acc) [] $ foldr (\s acc -> ("0"++s) : ("1"++s) : acc) [] [""]
foldr (\s acc -> ("0"++s) : ("1"++s) : acc) [] $ foldr (\s acc -> ("0"++s) : ("1"++s) : acc) [] $ ("0"++"") : ("1"++"") : [] 
foldr (\s acc -> ("0"++s) : ("1"++s) : acc) [] $  ("0"++"0"++"") : ("1"++"0"++"") : ("0"++"1"++"") : ("1"++"1"++"") : []
("0"++"0"++"0"++"") : ("1"++"0"++"0"++"") : ("0"++"1"++"0"++"") : ("1"++"1"++"0"++"") : ("0"++"0"++"1"++"") : ("1"++"0"++"1"++""):("0"++"1"++"1"++"") : ("1"++"1"++"1"++"") : []
["000", "100", "010", "110", "001", "101", "011", "111"]

gray 0 = [[]]
gray n = [x:xs | x <- ['0','1'], xs <- gray (n-1)]
gray 3
[x:xs | x <- ['0','1'], xs <- gray 2]
[x:xs | x <- ['0','1'], xs <- [x:xs | x <- ['0','1'], xs <- gray 1]]
[x:xs | x <- ['0','1'], xs <- [x:xs | x <- ['0','1'], xs <- [x:xs | x <- ['0','1'], xs <- gray 0]]]
[x:xs | x <- ['0','1'], xs <- [x:xs | x <- ['0','1'], xs <- [x:xs | x <- ['0','1'], xs <- [[]] ]]]
[x:xs | x <- ['0','1'], xs <- [x:xs | x <- ['0','1'], xs <- [[0], [1]]]]
[x:xs | x <- ['0','1'], xs <- [[00], [10], [01], [11]]]
[[000], [010], [001], [011],[100], [110], [101], [111]]

[
    Branch 0 
        (Branch 0 
            (Branch 0 Empty Empty) 
            Empty) 
        (Branch 0 
            Empty 
            Empty),
            
    Branch 0 
        (Branch 0 
            Empty 
            (Branch 0 Empty Empty)) 
        (Branch 0 
            Empty 
            Empty),
            
    Branch 0 
        (Branch 0 
            Empty 
            Empty) 
        (Branch 0 
            (Branch 0 
                Empty 
                Empty) 
            Empty),
            
    Branch 0 
        (Branch 0 
            Empty 
            Empty) 
        (Branch 0 
            Empty 
            (Branch 0 
                Empty 
                Empty))]
-}
