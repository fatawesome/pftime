module Data.Timeline.Overlapping where

import           Data.List           (sortOn)

import           Data.Timeline.Event

-- | OverlappingTimeline can have overlapping intervals.
newtype OverlappingTimeline t p =
  OverlappingTimeline
    { getOverlappingTimeline :: [Event t p] -- ^ Sorted list of intervals.
    }
  deriving (Show)

merge
  :: Ord t
  => OverlappingTimeline t p
  -> OverlappingTimeline t p
  -> OverlappingTimeline t p
merge (OverlappingTimeline xs) (OverlappingTimeline ys) = OverlappingTimeline (merge' xs ys)
  where
    merge' [] ys' = ys'
    merge' xs' [] = xs'
    merge' (e1@(Event i1 _):xs') (e2@(Event i2 _):ys')
      | i1 < i2 = e1 : merge' xs' (e2 : ys')
      | otherwise = e2 : merge' (e1 : xs') ys'

fromList
  :: Ord t
  => [Event t p]
  -> OverlappingTimeline t p
fromList xs = OverlappingTimeline (sortOn getInterval xs)
