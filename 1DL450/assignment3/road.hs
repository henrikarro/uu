import Control.Monad (forM)

main :: IO ()
main = do
  [numLines, length, goal] <- readInts
  intervals <- forM [1..numLines] (const readPair)
  print (days length intervals goal)

readInts :: IO [Int]
readInts = fmap (map read.words) getLine

readPair :: IO (Int, Int)
readPair = do
  [x, y] <- readInts
  return (x, y)

type Interval = (Int, Int)

days :: Int -> [Interval] -> Int -> Int
days length intervals goal = days' length intervals goal [] 0

days' :: Int -> [Interval] -> Int -> [Interval] -> Int -> Int
days' length [] goal intervalsSoFar day = if isSchedulable (goal + 1) intervalsSoFar 0 length then -1 else day
days' length (interval:intervals) goal intervalsSoFar day = if isSchedulable (goal + 1) intervalsSoFar 0 length then day' else day
  where day' = days' length intervals goal (addInterval interval intervalsSoFar) (day + 1)

addInterval :: Interval -> [Interval] -> [Interval]
addInterval (low1,hi1) [] = [(low1,hi1)]
addInterval (low1,hi1) ((low2,hi2):intervals) | hi1 < low2 = (low1,hi1):(low2,hi2):intervals
                                              | low1 > hi2 = (low2,hi2):(addInterval (low1,hi1) intervals)
                                              | low1 <= hi2 = addInterval ((min low1 low2),(max hi1 hi2)) intervals
                                              | low1 <= low2 && hi1 >= hi2 = (low1,hi1):intervals
                                              | low1 >= low2 && hi1 <= hi2 = (low2,hi2):intervals

isSchedulable :: Int -> [Interval] -> Int -> Int -> Bool
isSchedulable length [] start end = end - start >= length
isSchedulable _length _intervals start end | start >= end = False
isSchedulable length ((low,hi):intervals) start end | start < low = low - start >= length || isSchedulable length intervals hi end
                                                    | start >= low = isSchedulable length intervals hi end
