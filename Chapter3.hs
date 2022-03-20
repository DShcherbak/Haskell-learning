module Chapter3 where

main :: IO()
main = do
    putStrLn "Count to four for me:"
    putStr "one, two"
    putStr ", three, and"
    putStrLn " four!"


myGreeting :: String
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main2 :: IO ()
main2 = do
    putStrLn myGreeting
    putStrLn secondGreeting
        where 
            secondGreeting = concat [hello, " ", world]

area d = pi * (r * r)
    where r = d / 2