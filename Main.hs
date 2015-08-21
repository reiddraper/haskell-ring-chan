module Main where

import Criterion
import Criterion.Main
import Control.Monad (forever)
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)

type IntChan = MVar ()

type NumThreads = Int

startRing :: NumThreads -> IO (IntChan, IntChan)
startRing 0 = do
    starting <- newEmptyMVar
    to <- newEmptyMVar
    _ <- forkIO (worker starting to)
    return (to, starting)
startRing n = do
    reading <- newEmptyMVar
    (reading', writing') <- startRing (n - 1)
    _ <- forkIO (worker reading' reading)
    return (reading, writing')

worker :: IntChan -> IntChan -> IO ()
worker from to = takeMVar from >>= putMVar to

runBench :: Int -> IO ()
runBench numThreads = do
    (end, start) <- startRing numThreads
    putMVar start ()
    _ <- takeMVar end
    return ()

runBenchAlreadyStarted :: IntChan -> IntChan -> IO ()
runBenchAlreadyStarted end start = do
    putMVar start ()
    _ <- takeMVar end
    return ()

main :: IO ()
main = do
    --(end, start) <- startRing 100
    defaultMain [bench "100 threads" (nfIO (runBench 1000))]
