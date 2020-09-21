module Timeline where

import           Data.List      (sortOn)
import           Data.Monoid
import           Data.Semigroup

import Interval
import OverlappingTimeline

-- | Timeline cannot have overlapping intervals.
--
-- prop> not (haveConflicts (toList t))
newtype Timeline t e = Timeline
  { getTimeline :: [(Interval t, e)]    -- ^ Sorted list of intervals.
  } deriving (Show)

mergeIntervalsWithPayloads
  :: (e -> e -> e)
  -> (Interval t, e)
  -> (Interval t, e)
  -> [(Interval t, e)]
mergeIntervalsWithPayloads mergePayload i1 i2 =   

fromOverlappingTimeline 
  :: Ord t
  => (e -> e -> e)           -- ^ merge payload
  -> OverlappingTimeline t e -- ^ input timeline with conflicts
  -> Timeline t e            -- ^ timeline without conflicts
fromOverlappingTimeline mergePayload (OverlappingTimeline xs) = Timeline (resolveConflicts xs)
  where
    resolveConflicts [] = []
    resolveConflicts t = _
    -- ^ f [ ((1, 4), 'a'), ((2, 5), 'b') ] = [ ((1, 2), 'a'), ((2, 4), 'ab'), ((4, 5), 'ab') ]
    -- foldM (?)

toList :: Timeline t e -> [(Interval t, e)]
toList = getTimeline

fromListWith 
  :: Ord t 
  => (e -> e -> e)     -- ^ merge payload
  -> [(Interval t, e)] -- ^ list of intervals from which to create a Timeline
  -> Timeline t e      -- ^ new Timeline
fromListWith f lst = _

mergeTimeline 
  :: Ord t 
  => (e -> e -> e) 
  -> Timeline t e 
  -> Timeline t e
mergeTimeline f timeline = _

emptyTimeline :: Timeline t e
emptyTimeline = Timeline []