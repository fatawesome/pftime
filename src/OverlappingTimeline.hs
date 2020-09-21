module OverlappingTimeline where

import           Interval

-- | OverlappingTimeline can have overlapping intervals.
newtype OverlappingTimeline t e =
  OverlappingTimeline
    { getOverlappingTimeline :: [(Interval t, e)] -- ^ Sorted list of intervals.
    }
  deriving (Show)

merge 
  :: Ord t 
  => OverlappingTimeline t e 
  -> OverlappingTimeline t e 
  -> OverlappingTimeline t e
merge (OverlappingTimeline xs) (OverlappingTimeline ys) = OverlappingTimeline (merge' xs ys)
  where
    merge' [] ys = ys
    merge' xs [] = xs
    merge' ((i1, x):xs) ((i2, y):ys)
      | i1 < i2 = (i1, x) : merge' xs ((i2, y) : ys)
      | otherwise = (i2, y) : merge' ((i1, x) : xs) ys
