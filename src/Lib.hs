module Lib
  ( showTest,
  )
where

import Data.Monoid
import Data.Semigroup

data Interval t e = Interval (t, t) e deriving Show

newtype Timeline t e = Timeline [Interval t e] deriving Show

-- эксперименты 
-- newtype TimelineWithoutConflicts t e = TimelineWithoutConflicts (Timeline t e)

-- instance Semigroup (TimelineWithoutConflicts t e) where
--   TimelineWithoutConflicts left <> TimelineWithoutConflicts right = TimelineWithoutConflicts (t1 <> t2)

-- instance Semigroup (Timeline t e) where
--   Timeline x <> Timeline y = mconcat result
--     where
--       result :: [TimelineWithoutConflicts t e]
--       result = resolveConflicts x y

mergeIntervals ::
  Ord t =>
  (Interval t e -> Interval t e -> Interval t e) ->
  Interval t e ->
  Interval t e ->
  Timeline t e
mergeIntervals resolveConflict interval1 interval2
  | (start1 < start2) && (end1 < end2) =
    Timeline [interval1, interval2]
  | (start1 > start2) && (end1 > end2) =
    Timeline [interval2, interval1]
  | otherwise = Timeline [resolveConflict interval1 interval2]
  where
    Interval (start1, end1) _ = interval1
    Interval (start2, end2) _ = interval2

-- / реализация работает только если timeline1 и timeline2 не содержат в себе конфликтов.
-- / TODO: функция может вернуть таймлайн с конфликтами
concatTimelines ::
  Ord t =>
  (Interval t e -> Interval t e -> Interval t e) ->
  Timeline t e ->
  Timeline t e ->
  Timeline t e
concatTimelines resolvePayloadConflict timeline1 timeline2 
  = Timeline (concatMap (\ (Timeline i) -> i) timelineList)
  where
    Timeline intervals1 = timeline1
    Timeline intervals2 = timeline2
    timelineList = zipWith (mergeIntervals resolvePayloadConflict) intervals1 intervals2

testResolveConflict :: Interval t e -> Interval t e -> Interval t e
testResolveConflict left _ = left

test :: Timeline Integer [Char]
test = concatTimelines testResolveConflict timeline1 timeline2
  where
    timeline1 = Timeline [Interval (1, 2) "a1", Interval (4, 5) "b1"]
    timeline2 = Timeline [Interval (1, 3) "a2", Interval (4, 6) "b2"]

showTest :: IO ()
showTest = print test