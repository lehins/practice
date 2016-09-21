-- Compiled with:
--
-- stack --resolver lts-6.17 ghc --package criterion --package async --package stm --package fast-logger -- -threaded -rtsopts -with-rtsopts -N how-fast-logger.hs

import Criterion (bgroup,bench,whnfIO,whnf,nf)
import Criterion.Main (defaultMain)

import Control.Concurrent.Async (Concurrently (..), async, wait)
import Control.Concurrent.MVar (newMVar, withMVar, MVar)
import Control.Concurrent.STM
import Control.Monad            (unless, zipWithM_)
import Data.Char
import Data.Foldable            (traverse_)
import qualified Data.List as L
import Data.Monoid              ((<>))
import Data.String              (fromString)
import System.Log.FastLogger    (LoggerSet, LogStr, BufSize, newFileLoggerSet, pushLogStr,
                                 flushLogStr, rmLoggerSet)

main :: IO ()
main = do
  defaultMain [
    bgroup "threads: 26, bufSize: 16, entry: (4 x bufSize) " [
       bench "With STM"        (whnfIO $ runWithSTM 26 16 (16 * 4)),
       bench "With MVar"       (whnfIO $ runWithMVar 26 16 (16 * 4)),
       bench "With Bug" (whnfIO $ runWithout 26 16 (16 * 4)) ], 
    bgroup "threads: 26, bufSize: 16, entry: (bufSize / 4) " [
       bench "With STM"        (whnfIO $ runWithSTM 26 16 (16 `div` 4)),
       bench "With MVar"       (whnfIO $ runWithMVar 26 16 (16 `div` 4)),
       bench "With Bug" (whnfIO $ runWithout 26 16 (16 `div` 4)) ], 
    bgroup "threads: 4, bufSize: 4096, entry: (4 x bufSize) " [
       bench "With STM"        (whnfIO $ runWithSTM 4 4096 (4096 * 4)),
       bench "With MVar"       (whnfIO $ runWithMVar 4 4096 (4096 * 4)),
       bench "With Bug" (whnfIO $ runWithout 4 4096 (4096 * 4)) ],
    bgroup "threads: 4, bufSize: 4096, entry: (bufSize / 4) " [
       bench "With STM"        (whnfIO $ runWithSTM 4 4096 (4096 `div` 4)),
       bench "With MVar"       (whnfIO $ runWithMVar 4 4096 (4096 `div` 4)),
       bench "With Bug" (whnfIO $ runWithout 4 4096 (4096 `div` 4)) ],
    bgroup "threads: 16, bufSize: 4096, entry: (4 x bufSize) " [
       bench "With STM"        (whnfIO $ runWithSTM 16 4096 (4096 * 4)),
       bench "With MVar"       (whnfIO $ runWithMVar 16 4096 (4096 * 4)),
       bench "With Bug" (whnfIO $ runWithout 16 4096 (4096 * 4)) ],
    bgroup "threads: 16, bufSize: 4096, entry: (bufSize / 4) " [
       bench "With STM"        (whnfIO $ runWithSTM 16 4096 (4096 `div` 4)),
       bench "With MVar"       (whnfIO $ runWithMVar 16 4096 (4096 `div` 4)),
       bench "With Bug" (whnfIO $ runWithout 16 4096 (4096 `div` 4)) ]
    ]

runWithSTM :: Int -> BufSize -> Int -> IO ()
runWithSTM threads bufSize msgSize = do
  let logFile = getLogFileName "withSTM" threads bufSize msgSize
  chan <- newTChanIO
  loggerSet <- newFileLoggerSet bufSize logFile
  worker <- async (stmLogWriter chan loggerSet)
  runConcurrently $
    traverse_ (Concurrently . runLoggerWithSTM chan msgSize) [1..threads]
  atomically $ writeTChan chan Nothing -- indicate the end
  wait worker
  rmLoggerSet loggerSet


stmLogWriter :: TChan (Maybe LogStr) -> LoggerSet -> IO ()
stmLogWriter chan loggerSet = do
  strM <- atomically $ readTChan chan
  case strM of
    Nothing -> return ()
    Just str -> do pushLogStr loggerSet str
                   flushLogStr loggerSet
                   stmLogWriter chan loggerSet


runLoggerWithSTM :: TChan (Maybe LogStr) -> Int -> Int -> IO ()
runLoggerWithSTM chan msgSize tId =
  atomically $ writeTChan chan (Just (getStr msgSize tId))


runWithMVar :: Int -> BufSize -> Int -> IO ()
runWithMVar threads bufSize msgSize = do
  let logFile = getLogFileName "withMVar" threads bufSize msgSize
  baton <- newMVar ()
  loggerSet <- newFileLoggerSet bufSize logFile
  runConcurrently $
    traverse_ (Concurrently . runLoggerWithMVar baton msgSize loggerSet) [1..threads]
  rmLoggerSet loggerSet


runLoggerWithMVar :: MVar () -> Int -> LoggerSet -> Int -> IO ()
runLoggerWithMVar baton msgSize loggerSet tId =
  withMVar baton (\() -> do
                     pushLogStr loggerSet (getStr msgSize tId)
                     flushLogStr loggerSet)


runWithout :: Int -> BufSize -> Int -> IO ()
runWithout threads bufSize msgSize = do
  let logFile = getLogFileName "without" threads bufSize msgSize
  loggerSet <- newFileLoggerSet bufSize logFile
  runConcurrently $ traverse_ (Concurrently . runLoggerDirectly msgSize loggerSet) [1..threads]
  rmLoggerSet loggerSet


runLoggerDirectly :: Int -> LoggerSet -> Int -> IO ()
runLoggerDirectly msgSize loggerSet tId = do
  pushLogStr loggerSet (getStr msgSize tId)
  flushLogStr loggerSet


getLogFileName :: String -> Int -> BufSize -> Int -> String
getLogFileName prefix threads bufSize msgSize =
  (L.intercalate "_" ["log", prefix, show threads, show bufSize, show msgSize]) ++ ".log"


getStr :: Int -> Int -> LogStr
getStr msgSize tId = fromString (replicate msgSize (chr (tId + 64)) <> "\n")
