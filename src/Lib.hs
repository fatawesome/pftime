module Lib
  ( showTest,
  )
where

import Data.Monoid
import Data.Semigroup
import Data.List

data Interval t e = Interval (t, t) e deriving (Show)

instance (Eq t) => Eq (Interval t e) where
  Interval (start1, end1) payload1 == Interval (start2, end2) payload2
    = start1 == start2 && end1 == end2

-- | Instead, Interval should derive Ord with custom comparator, I suppose
intervalOrdComparator :: Ord t => Interval t e -> Interval t e -> Ordering
intervalOrdComparator i1 i2
  | start1 <= start2 = LT
  | otherwise = GT
  where
    Interval (start1, _) _ = i1
    Interval (start2, _) _ = i2

-- | Timeline cannot have cconflicts inside itself 
newtype Timeline t e = Timeline [Interval t e] deriving Show

intervalsHaveConflict :: Ord t => Interval t e -> Interval t e -> Bool
intervalsHaveConflict i1 i2
  | end1 < start2 || start1 > end2 = False
  | otherwise = True
  where
    Interval (start1, end1) _ = i1
    Interval (start2, end2) _ = i2

timelineHasConflict :: Ord t => Timeline t e -> Bool
timelineHasConflict timeline = res
  where
    Timeline intervals = timeline
    res = any (\x -> any (\y -> (x /= y) && intervalsHaveConflict x y) intervals) intervals

intervalAccumulator :: 
  Ord t =>
  (Interval t e -> Interval t e -> Interval t e) ->
  Interval t e -> 
  [Interval t e] -> 
  [Interval t e]
intervalAccumulator _ interval [] = [interval]
intervalAccumulator resolveConflict interval intervalList
  | intervalsHaveConflict x interval 
    = resolveConflict x interval : xs
  | otherwise = interval : intervalList
  where
    x:xs = intervalList
    

-- | First, concat interval lists and sort them in the acsending order on interval start value.
-- | Then one by one (better recursively ofc) resolve conflicts, if present.
timelineUnion :: 
  Ord t =>
  (Interval t e -> Interval t e -> Interval t e) ->
  Timeline t e ->
  Timeline t e ->
  Timeline t e
timelineUnion resolveConflict t1 t2 = Timeline sortedIntervals
  where
    Timeline intervalList1 = t1
    Timeline intervalList2 = t2
    sortedIntervals = sortBy intervalOrdComparator (intervalList1 <> intervalList2)
    resolvedIntervals = foldr (intervalAccumulator resolveConflict) [] sortedIntervals
    

testResolveConflict :: Interval t e -> Interval t e -> Interval t e
testResolveConflict left _ = left

test :: Timeline Integer [Char]
test = timelineUnion testResolveConflict timeline1 timeline2
  where
    timeline1 = Timeline [Interval (1, 2) "a1", Interval (5, 6) "b1"]
    timeline2 = Timeline [Interval (3, 4) "a2", Interval (4, 6) "b2"]

showTest :: IO ()
showTest = print test