module Timeline where

import           Control.Monad
import           Data.List           (sortOn)
import           Data.Monoid
import           Data.Semigroup

import           Interval
import           OverlappingTimeline

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

fromOverlappingTimeline
  :: Ord t
  => (e -> e -> e)           -- ^ merge payload
  -> OverlappingTimeline t e -- ^ input timeline with conflicts
  -> Timeline t e            -- ^ timeline without conflicts
fromOverlappingTimeline mergePayload (OverlappingTimeline xs) = Timeline (resolveConflicts xs)
  where
    resolveConflicts [] = []
    resolveConflicts (t:ts)  = foldM (concatIntervalsWithPayloads mergePayload) t ts
    -- ^ f [ ((1, 4), 'a'), ((2, 5), 'b') ] = [ ((1, 2), 'a'), ((2, 4), 'ab'), ((4, 5), 'ab') ]
    -- foldM (?)

toList :: Timeline t e -> [(Interval t, e)]
toList = getTimeline

-- fromListWith
--   :: Ord t
--   => (e -> e -> e)     -- ^ merge payload
--   -> [(Interval t, e)] -- ^ list of intervals from which to create a Timeline
--   -> Timeline t e      -- ^ new Timeline
-- fromListWith f lst = _

-- mergeTimeline
--   :: Ord t
--   => (e -> e -> e)
--   -> Timeline t e
--   -> Timeline t e
-- mergeTimeline f timeline = _

emptyTimeline :: Timeline t e
emptyTimeline = Timeline []

-- | just for testing purposes
res = fromOverlappingTimeline (++) (OverlappingTimeline [(Interval (1, 4), "a"), (Interval (2, 5), "b")])