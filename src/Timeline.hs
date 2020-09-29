  {-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

module Timeline where

import           Interval
import           OverlappingTimeline


-- | Timeline cannot have overlapping intervals.
--
-- prop> not (haveConflicts (toList t))
newtype Timeline t e = Timeline
  { getTimeline :: [(Interval t, e)]    -- ^ Sorted list of intervals.
  } deriving (Show)

insert
  :: Ord t
  => (e -> e -> e)
  -> (Interval t, e)
  -> Timeline t e
  -> Timeline t e
insert _ el (Timeline []) = Timeline [el]
insert mergePayload el@(Interval (left, right), e) (Timeline (x@(Interval (xleft, xright), eX) : xs))
  | right <= xleft
    = Timeline (el:x:xs)
  | left < xleft && right < xright && right > xleft
    = Timeline (
      [ (Interval (left, xleft), e)
      , (Interval (xleft, right), mergePayload eX e)
      , (Interval (right, xright), eX)
      ] <> xs
    )
  | left == xleft && right < xright
    = Timeline (
      [ (Interval (left, right), mergePayload eX e)
      , (Interval (right, xright), e)
      ] <> xs
    )
  | left == xleft && right == xright
    = Timeline ((Interval (left, right), mergePayload eX e) : xs)
  | left > xleft && right == xright
    = Timeline (
      [ (Interval (xleft, left), eX)
      , (Interval (left, right), mergePayload eX e)
      ] <> xs
    )
  | left > xleft && right > xright && left < xright
    = Timeline (
      [ (Interval (xleft, left), eX)
      , (Interval (left, xright), mergePayload eX e)
      ] <> getTimeline (insert mergePayload (Interval (xright, right), e) (Timeline xs))
    )
  | left >= xright 
    = Timeline (x : getTimeline (insert mergePayload el (Timeline xs)))
  | left == xleft && right > xright
    = Timeline (
      (Interval (left, xright), mergePayload eX e) 
      : getTimeline (insert mergePayload (Interval (xright, right), e) (Timeline xs))
      )
  | left > xleft && right < xright
    = Timeline (
      [ (Interval (xleft, left), eX)
      , (Interval (left, right), mergePayload eX e)
      , (Interval (right, xright), eX)
      ] <> xs
    )
  | left < xleft && right > xright
    = Timeline (
      [ (Interval (left, xleft), e)
      , (Interval (xleft, xright), mergePayload eX e)
      ] <> getTimeline (insert mergePayload (Interval (xright, right), e) (Timeline xs))
    )

mergeTimeline
  :: Ord t
  => (e -> e -> e)
  -> Timeline t e
  -> Timeline t e
  -> Timeline t e
mergeTimeline f (Timeline xs) (Timeline ys) = fromListWith f (xs <> ys)

-- take
-- takeWhile
-- difference
-- intersection

-- | Create Timeline without conflicts from Overlapping Timleine
-- | O((n^2)/2)
fromOverlappingTimeline
  :: Ord t
  => (e -> e -> e)           -- ^ merge payload
  -> OverlappingTimeline t e -- ^ input timeline with conflicts
  -> Timeline t e            -- ^ timeline without conflicts
fromOverlappingTimeline mergePayload (OverlappingTimeline xs) = resolveConflicts xs
  where
    resolveConflicts [] = emptyTimeline
    resolveConflicts (t:ts) = foldr (insert mergePayload) (Timeline [t]) ts

toList :: Timeline t e -> [(Interval t, e)]
toList = getTimeline

-- | As fromOverlappingTimeline, but from list
-- | O((n^2)/2)
fromListWith
  :: Ord t
  => (e -> e -> e)     -- ^ merge payload
  -> [(Interval t, e)] -- ^ list of intervals from which to create a Timeline
  -> Timeline t e      -- ^ new Timeline
fromListWith f lst = fromOverlappingTimeline f (fromList lst)

emptyTimeline :: Timeline t e
emptyTimeline = Timeline []

-- | just for testing purposes
res = fromOverlappingTimeline (++) (OverlappingTimeline [(Interval (1, 4), "a"), (Interval (2, 5), "b"), (Interval (6, 9), "b")])
res2 = fromListWith (++) [(Interval (2, 5), "b"), (Interval (1, 4), "a")]
res3 = mergeTimeline (++) (Timeline [(Interval (1, 4), "a"), (Interval (5, 7), "b")]) (Timeline [(Interval (2, 6), "a"), (Interval (7, 9), "b")])
