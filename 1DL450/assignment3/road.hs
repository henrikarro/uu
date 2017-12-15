import Control.Monad (forM)
import Test.HUnit (Test(..), assertEqual, runTestTT)

main :: IO ()
main = do
  [numLines, length, goal] <- readInts
  intervals <- forM [1..numLines] (const readPair)
  print (days length intervals goal)

readInts :: IO [Integer]
readInts = fmap (map read.words) getLine

readPair :: IO (Integer, Integer)
readPair = do
  [x, y] <- readInts
  return (x, y)

type Interval = (Integer, Integer)

days :: Integer -> [Interval] -> Integer -> Integer
days length intervals goal = days' length intervals goal [] 0
  where days' :: Integer -> [Interval] -> Integer -> [Interval] -> Integer -> Integer
        days' length [] goal intervalsSoFar day = if isSchedulable (goal + 1) intervalsSoFar 0 length then -1 else day
        days' length (interval:intervals) goal intervalsSoFar day =
          if isSchedulable (goal + 1) intervalsSoFar 0 length then day' else day
          where day' = days' length intervals goal (addInterval interval intervalsSoFar) (day + 1)

addInterval :: Interval -> [Interval] -> [Interval]
addInterval (low,hi) intervals | hi <= low = intervals
addInterval (low,hi) [] = [(low,hi)]
addInterval (low1,hi1) ((low2,hi2):intervals) | hi1 < low2 = (low1,hi1):(low2,hi2):intervals
                                              | low1 > hi2 = (low2,hi2):(addInterval (low1,hi1) intervals)
                                              | low1 <= hi2 = addInterval ((min low1 low2),(max hi1 hi2)) intervals
                                              | low1 <= low2 && hi1 >= hi2 = (low1,hi1):intervals
                                              | low1 >= low2 && hi1 <= hi2 = (low2,hi2):intervals

isSchedulable :: Integer -> [Interval] -> Integer -> Integer -> Bool
isSchedulable length [] start end = end - start >= length
isSchedulable _length _intervals start end | start >= end = False
isSchedulable length ((low,hi):intervals) start end | start < low = low - start >= length || isSchedulable length intervals hi end
                                                    | start >= low = isSchedulable length intervals hi end


-- ===================================================================
-- HUnit Test Cases
-- ===================================================================

daysTestCase (expected, length, intervals, goal) =
  TestCase (assertEqual (show expected ++ " " ++ show length)
            expected (days length intervals goal))

daysTestData :: [(Integer, Integer, [Interval], Integer)]
daysTestData = [
  -- Assignment example test cases
  (-1, 30, [(1,5),(11,27),(2,14),(18,28)], 0),
  (-1, 30, [(1,5),(11,27),(2,14),(18,28)], 1),
  (4, 30, [(1,5),(11,27),(2,14),(18,28)], 2),
  (3, 30, [(1,5),(11,27),(2,14),(18,28)], 3),
  (3, 30, [(1,5),(11,27),(2,14),(18,28)], 4),
  (3, 30, [(1,5),(11,27),(2,14),(18,28)], 5),
  (2, 30, [(1,5),(11,27),(2,14),(18,28)], 6),
  (2, 30, [(1,5),(11,27),(2,14),(18,28)], 7),
  (2, 30, [(1,5),(11,27),(2,14),(18,28)], 24),
  (1, 30, [(1,5),(11,27),(2,14),(18,28)], 25),
  (1, 30, [(1,5),(11,27),(2,14),(18,28)], 26),
  (1, 30, [(1,5),(11,27),(2,14),(18,28)], 29),
  (0, 30, [(1,5),(11,27),(2,14),(18,28)], 30),
  -- My test cases
  (1, 1, [(0,1)], 0),
  (-1, 1, [(1,1)], 0),
  (1, 2, [(0,2)], 0),
  (-1, 2, [(1,2)], 0),
  (-1, 10, [(1,5),(6,10)], 0),
  (2, 100, [(1,10),(20,100)], 10),
  -- Discovered by PropEr
  (2, 1000, [(1,361),(458,900),(1,225)], 100),
  -- Assignment grading test cases
  (2, 30, [(1,5),(11,27),(2,14),(18,28)], 6),
  (-1, 30, [(1,5),(11,27),(2,14),(18,28)], 1),
  (0, 1, [(1,1)], 1),
  (-1, 1, [(1,1)], 0),
  (-1, 2, [(1,1)], 1),
  (-1, 1, [(1,1),(1,1)], 0),
  (6, 58, [(57,57),(6,42),(19,23),(41,42),(15,36),(46,53),(8,46),(2,14),(58,58),(57,58),(17,28),(16,35),(23,26),(20,32)], 8),
  (3, 2573, [(119,1209),(482,1901),(1729,2463),(66,2150),(602,976),(1176,2323),(1875,2370),(1703,2463),(1912,2214),(887,1363),(1765,2242),(2278,2505),(1452,2111),(34,1782),(2189,2229),(1825,2546),(1247,1851),(2287,2328),(910,2067),(2347,2395),(1519,1672),(1253,1509),(1416,1750),(205,948),(1479,1812),(207,906),(660,840),(2263,2401),(2406,2458),(2378,2448),(862,2122),(2439,2545),(1064,1680),(702,761),(2055,2275),(1784,2405),(1170,2446),(1072,1598),(1455,2100),(2507,2531),(1094,1608),(2184,2270),(1257,2519),(2058,2566),(360,1368),(2015,2221),(1846,2046),(355,2233),(2403,2436),(1407,1985),(608,1385),(781,1377),(543,1341),(294,1454),(82,1366),(515,1504),(700,2144),(361,2231),(56,281),(770,843),(781,2367),(1726,1783),(2092,2241),(740,2321),(1579,2408),(706,2490),(239,2215),(2487,2512),(978,1756),(1422,1470),(244,2280),(1139,1214),(1227,1791),(1510,2266),(1451,2011),(2321,2444),(265,1722),(2195,2412),(2394,2465),(1251,2184),(2548,2561),(2333,2509)], 349)
  ]

daysTests =  zipWith (\n t -> TestLabel ("daysTest " ++ show n) t) [1..] (map daysTestCase daysTestData)

addIntervalTestCase (expected, interval, intervals) =
  TestCase (assertEqual (show expected ++ " " ++ show interval)
           expected (addInterval interval intervals))

addIntervalTestData = [
  ([(5,10)], (5,10), []),
    ([(5,10),(11,15)], (11,15), [(5,10)]),
    ([(5,15)], (10,15), [(5,10)]),
    ([(5,15)], (7,15), [(5,10)]),
    ([(5,10)], (6,9), [(5,10)]),
    ([(5,10)], (5,10), [(5,10)]),
    ([(1,10)], (1,6), [(5,10)]),
    ([(1,10)], (1,5), [(5,10)]),
    ([(1,4),(5,10)], (1,4), [(5,10)]),
    ([(1,20)], (1,20), [(1,4),(5,10)]),
    ([(5,10), (11,15)], (5,10), [(11,15)]),
    ([], (1,1), []),
    ([(2,3)], (1,1), [(2,3)]),
    ([(1,2)], (3,3), [(1,2)])
  ]

addIntervalTests = zipWith (\n t -> TestLabel ("addIntervalTest " ++ show n) t) [1..]
                   (map addIntervalTestCase addIntervalTestData)
                   
runTests = runTestTT (TestList (daysTests ++ addIntervalTests))
