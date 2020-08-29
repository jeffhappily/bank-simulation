{- stack script
    --resolver lts-16.10
    --install-ghc
    --ghc-options -Wall
    --package async
    --package conduit
    --package random
    --package ghc
    --package mtl
-}
-- {-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE RecordWildCards #-}
import           Control.Monad
import           Control.Monad.State
import           Data.List
import           System.Random
import           Util

type Second = Double
type Rate = Double

data Customer
  = CYellow
  | CRed
  | CBlue
  deriving (Eq, Enum)

instance Show Customer where
  show CYellow = "Customer Yellow"
  show CRed    = "Customer Red"
  show CBlue   = "Customer Blue"

f :: (Floating a) => a -> a
f t = 1 - exp ((- t) / alpha')
  where
    alpha' = 100

-- Inverse function of f, return time (in seconds) given a probability
f' :: (Floating a, RealFrac a) => a -> a
f' t = fromIntegral . round $ -alpha' * log (1 - t)
  where
    alpha' = 100

class ProcessingTime a where
  alpha :: Num b => a -> b
  beta :: Num b => a -> b

  ptFn :: a -> Double -> Second
  ptFn a x = p * (x ^ (alpha a - 1)) * ((1 - x) ^ (beta a - 1))
    where
      p = 200

  meanTime :: a -> Second
  meanTime a = ptFn a meanX
    where
      meanX = alpha a / (alpha a + beta a)

  modeTime :: a -> Second
  modeTime a = ptFn a modeX
    where
      modeX = (alpha a - 1) / (alpha a + beta a - 2)

instance ProcessingTime Customer where
  alpha CYellow = 2
  alpha CRed    = 2
  alpha CBlue   = 5

  beta CYellow = 5
  beta CRed    = 2
  beta CBlue   = 1

rateOf :: Second -> Rate
rateOf t = 1 / t

printPrefix :: (Show a) => String -> a -> IO ()
printPrefix pre a = putStr pre >> putStr ": " >> print a

data Time =
  Time
    { prevFinish :: Second
    , prevClock  :: Second
    , prevQueue :: [Second] -- list of finish time
    , waitTime   :: [Second]
    , queueLength :: [Int]
    }
    deriving (Show)

mkTime :: Time
mkTime = Time 0 0 [] [] []

getWaitTimes :: Int -> Customer -> IO [Second]
getWaitTimes = (fmap waitTime .) . simulateQueue

getQueueLengths :: Int -> Customer -> IO [Int]
getQueueLengths = (fmap queueLength .) . simulateQueue

simulateQueue :: Int -> Customer -> IO Time
simulateQueue cases c = foldM g mkTime [1..cases]
  where
    g :: Time -> Int -> IO Time
    g t@(Time {..}) _ = do
      x <- liftIO $ randomIO
      let
        nextArrivalTime = f' x
        clockTime = nextArrivalTime + prevClock
        startTime = if clockTime < prevFinish
                      then prevFinish
                      else prevFinish + (clockTime - prevFinish)
        processTime = ptFn c x
        finish = startTime + processTime
        newWaitTime = finish - clockTime
        newQueue = finish : filter (> clockTime) prevQueue

      return $
        t { prevFinish = finish
          , prevClock = clockTime
          , prevQueue = newQueue
          , waitTime = newWaitTime : waitTime
          , queueLength = length newQueue : queueLength
          }

mean :: (Real a, Fractional b) => [a] -> b
mean xs = realToFrac (sum xs) / genericLength xs

task1 :: IO ()
task1 = do
  putStrLn "Task 1"
  wt <- getWaitTimes 1000000 CYellow
  printPrefix "Mean" $ mean wt
  printPrefix "Mode" $ maximum wt

task2 :: IO ()
task2 = do
  putStrLn "Task 2"
  ql <- getQueueLengths 10000 CRed
  printPrefix "Mean" $ mean ql
  printPrefix "Mode" $ maximum ql

task3 :: IO ()
task3 = do
  putStrLn "Task 3"
  wts <- mapM (getWaitTimes 1000000) $ enumFrom CYellow

  let (c, _) = minWith
          (\(_, xs) ->
            let meanTime = mean xs
                modeTime = maximum xs
            in abs $ meanTime - modeTime)
          $ zip (enumFrom CYellow) wts
  print c

main :: IO ()
main = do
  -- task1
  task2
  -- task3C
