{- stack script
    --resolver lts-16.10
    --install-ghc
    --ghc-options -Wall
    --package random
    --package ghc
    --package mtl
-}
{-# LANGUAGE RecordWildCards #-}
import           Control.Monad
import           Control.Monad.State
import           Data.List
import           System.Random
import           Util

type Second = Double

data Customer
  = CYellow
  | CRed
  | CBlue
  deriving (Eq, Enum)

instance Show Customer where
  show CYellow = "Customer Yellow"
  show CRed    = "Customer Red"
  show CBlue   = "Customer Blue"

-- Returns the probability that the next customer arrives after *t* second(s)
f :: (Floating a) => a -> a
f t = 1 - exp ((- t) / alpha')
  where
    alpha' = 100

-- Inverse function of f, return time (in seconds) given a probability
f' :: (Floating a, RealFrac a) => a -> Integer
f' t = round $ -alpha' * log (1 - t)
  where
    alpha' = 100

class ProcessingTime a where
  alpha :: a -> Integer
  beta :: a -> Integer

  ptFn :: a -> Double -> Second
  ptFn a x = p * (x ^ (alpha a - 1)) * ((1 - x) ^ (beta a - 1))
    where
      p = 200

instance ProcessingTime Customer where
  alpha CYellow = 2
  alpha CRed    = 2
  alpha CBlue   = 5

  beta CYellow = 5
  beta CRed    = 2
  beta CBlue   = 1

printPrefix :: (Show a) => String -> a -> IO ()
printPrefix pre a = putStr pre >> putStr ": " >> print a

data Time =
  Time
    { prevFinish  :: Second
    , prevClock   :: Second
    , prevQueue   :: [Second] -- list of finish time
    , waitTime    :: [Second]
    , queueLength :: [Int]
    }
    deriving (Show)

mkTime :: Time
mkTime = Time 0 0 [] [] []

getWaitTimes :: Int -> Customer -> IO [Second]
getWaitTimes = (fmap waitTime .) . simulateQueue

getQueueLengths :: Int -> Customer -> IO [Int]
getQueueLengths = (fmap queueLength .) . simulateQueue

-- Simulates *c* customer(s) coming in to the bank, and produces the
-- wait time and queue length for all the customers, referencing this video
-- https://www.youtube.com/watch?v=aqAPH5GZBLg
simulateQueue :: Int -> Customer -> IO Time
simulateQueue cases c = foldM g mkTime [1..cases]
  where
    g :: Time -> Int -> IO Time
    g t@(Time {..}) _ = do
      x <- liftIO $ randomIO
      let
        -- Randomly generate the customer arrival time and processing time
        -- for the customer
        nextArrivalTime = fromIntegral $ f' x
        processTime = ptFn c x
        -- The time that the customer arrived
        clockTime = nextArrivalTime + prevClock
        -- The time that the customer speaks to the teller
        startTime = if clockTime < prevFinish
                      then prevFinish
                      else prevFinish + (clockTime - prevFinish)
        finish = startTime + processTime
        newWaitTime = finish - clockTime
        -- remove finished customer
        newQueue = finish : filter (> clockTime) prevQueue

      return $
        t { prevFinish = finish
          , prevClock = clockTime
          , prevQueue = newQueue
          , waitTime = newWaitTime : waitTime
          , queueLength = length newQueue : queueLength
          }

mean :: (Real a) => [a] -> Double
mean xs = realToFrac (sum xs) / genericLength xs

task1 :: Int -> IO ()
task1 cases = do
  putStrLn "----"
  putStrLn "Task 1"
  wt <- getWaitTimes cases CYellow
  printPrefix "Mean" $ mean wt
  printPrefix "Mode" $ maximum wt

task2 :: Int -> IO ()
task2 cases = do
  putStrLn "----"
  putStrLn "Task 2"
  ql <- getQueueLengths cases CRed
  printPrefix "Mean" $ mean ql
  printPrefix "Mode" $ maximum ql

task3 :: Int -> IO ()
task3 cases = do
  putStrLn "----"
  putStrLn "Task 3"
  wts <- mapM (getWaitTimes cases) $ enumFrom CYellow

  let (c, _) = minWith
          (\(_, xs) ->
            let meanTime = mean xs
                modeTime = maximum xs
            in abs $ meanTime - modeTime)
          $ zip (enumFrom CYellow) wts
  print c

main :: IO ()
main = do
  let cases = 1000000
  task1 cases
  task2 cases
  task3 cases
