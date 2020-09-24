module Timeline where

import           Control.Monad
import           Data.Monoid
import           Data.Semigroup

import           Interval
import           OverlappingTimeline

import           Data.List           (sort)

-- | Timeline cannot have overlapping intervals.
--
-- prop> not (haveConflicts (toList t))
newtype Timeline t e = Timeline
  { getTimeline :: [(Interval t, e)]    -- ^ Sorted list of intervals.
  } deriving (Show)

-- | Concat ordered intervals with payload (pair 1 < pair2)
-- | If intervals overlap, then create middle interval, which will have merged payload.
concatIntervalsWithPayloads
  :: Ord t
  => (e -> e -> e)
  -> (Interval t, e)
  -> (Interval t, e)
  -> [(Interval t, e)]
concatIntervalsWithPayloads
  mergePayload
  pair1@(Interval (left1, right1), e1)
  pair2@(Interval (left2, right2), e2)
  | right1 > left2 =
    [ (Interval (left1, left2), e1)
    , (Interval (left2, right1), mergePayload e1 e2)
    , (Interval (right1, right2), e2)
    ]
  | otherwise = [pair1, pair2]

-- LATEST 2 FUNCTIONS
-- !!!WIP!!! warning
-- | Resolve conflict so that the result is in the bounds of lesser interval
resolveConflictToLeft
  :: Ord t
  => (e -> e -> e)
  -> (Interval t, e)
  -> (Interval t, e)
  -> [(Interval t, e)]
resolveConflictToLeft resolve (i1, e1) (i2, e2)
  | end1 < start2 = [(Interval (start1, end1), e1)]
  | otherwise =
    [ (Interval (start1, start2), e1)
    , (Interval (start2, end1), resolve e1 e2)
    ]
  where
    [left, right] = sort [i1, i2]
    Interval (start1, end1) = left
    Interval (start2, end2) = right

insert
  :: Ord t
  => (e -> e -> e)
  -> (Interval t, e)
  -> Timeline t e
  -> Timeline t e
insert mergePayload elem (Timeline xs)
  = Timeline (foldl (<>) [] (map (resolveConflictToLeft mergePayload elem) xs))

fromOverlappingTimeline
  :: Ord t
  => (e -> e -> e)           -- ^ merge payload
  -> OverlappingTimeline t e -- ^ input timeline with conflicts
  -> Timeline t e            -- ^ timeline without conflicts
fromOverlappingTimeline mergePayload (OverlappingTimeline xs) = Timeline (resolveConflicts xs)
  where
    resolveConflicts [] = []
    resolveConflicts (t:ts) = foldM (concatIntervalsWithPayloads mergePayload) t ts
    -- ^ f [ ((1, 4), 'a'), ((2, 5), 'b') ] = [ ((1, 2), 'a'), ((2, 4), 'ab'), ((4, 5), 'ab') ]
    -- foldM (?)

toList :: Timeline t e -> [(Interval t, e)]
toList = getTimeline

fromListWith
  :: Ord t
  => (e -> e -> e)     -- ^ merge payload
  -> [(Interval t, e)] -- ^ list of intervals from which to create a Timeline
  -> Timeline t e      -- ^ new Timeline
fromListWith f lst = fromOverlappingTimeline f (fromList lst)

mergeTimeline
  :: Ord t
  => (e -> e -> e)
  -> Timeline t e
  -> Timeline t e
  -> Timeline t e
mergeTimeline f (Timeline xs) (Timeline ys) = fromListWith f (xs <> ys)

emptyTimeline :: Timeline t e
emptyTimeline = Timeline []

-- | just for testing purposes
res = fromOverlappingTimeline (++) (OverlappingTimeline [(Interval (1, 4), "a"), (Interval (2, 5), "b"), (Interval (6, 9), "b")])
res2 = fromListWith (++) [(Interval (2, 5), "b"), (Interval (1, 4), "a")]
res3 = mergeTimeline (++) (Timeline [(Interval (1, 4), "a"), (Interval (5, 7), "b")]) (Timeline [(Interval (2, 6), "a"), (Interval (7, 9), "b")])

res5 = insert (++) (Interval (2, 6), "a") (Timeline [(Interval (1, 4), "a"), (Interval (5, 7), "b")])
