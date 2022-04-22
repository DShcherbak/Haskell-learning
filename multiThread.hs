module Main where

import Control.Concurrent (forkIO, threadDelay)
import Data.Foldable (for_)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)

sleepMs n = threadDelay (n * 1000)

printMessagesFrom name = for_ [1..3] printMessage
    where printMessage i = do
            sleepMs 1
            putStrLn (name ++ " number " ++ show i)

main = do
    printMessagesFrom "main"

    forkIO (printMessagesFrom "fork")

    forkIO (do
        putStrLn "starting!"
        sleepMs 5
        putStrLn "ending!")

    sleepMs 10


main2 = do
    result <- newEmptyMVar
    result2  <- newEmptyMVar
    

    forkIO (do
        -- Pretend there is some actual work to do.
        sleepMs 5
        val <- takeMVar result
        putStrLn $ "Got " ++ show val ++ "...\n Calculated result!"
        putMVar result2 $ val + 5)

    forkIO(do 
        putMVar result 37
        putStrLn "37 in")
    

    putStrLn "Waiting..."
    value <- takeMVar result2
    putStrLn ("The answer is: " ++ show value)