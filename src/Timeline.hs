  {-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

module Timeline where

import           Control.Monad

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

insert
  :: Ord t
  => (e -> e -> e)
  -> (Interval t, e)
  -> Timeline t e
  -> Timeline t e
insert _ el (Timeline []) = Timeline [el]
insert mergePayload el@(Interval (left, right), e) (Timeline (x@(Interval (xleft, xright), eX) : xs))
  | right < xleft
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
  | left > xright 
    = Timeline (x : getTimeline (insert mergePayload el (Timeline xs)))
  | left == xleft && right > xright
    = Timeline (
      (Interval (left, xright), mergePayload eX e) 
      : getTimeline (insert mergePayload (Interval (xright, right), e) (Timeline xs))
      )
  | left > xleft && right < xright
    = Timeline (
      [ (Interval (left, xleft), eX)
      , (Interval (xleft, right), mergePayload eX e)
      , (Interval (right, xright), eX)
      ] <> xs
    )
  | left < xleft && right > xright
    = Timeline (
      [ (Interval (left, xleft), e)
      , (Interval (xleft, right), mergePayload eX e)
      ] <> getTimeline (insert mergePayload (Interval (xright, right), e) (Timeline xs))
    )

mergeTimeline
  :: Ord t
  => (e -> e -> e)
  -> Timeline t e
  -> Timeline t e
  -> Timeline t e
mergeTimeline f (Timeline xs) (Timeline ys) = fromListWith f (xs <> ys)

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

emptyTimeline :: Timeline t e
emptyTimeline = Timeline []

-- | just for testing purposes
res = fromOverlappingTimeline (++) (OverlappingTimeline [(Interval (1, 4), "a"), (Interval (2, 5), "b"), (Interval (6, 9), "b")])
res2 = fromListWith (++) [(Interval (2, 5), "b"), (Interval (1, 4), "a")]
res3 = mergeTimeline (++) (Timeline [(Interval (1, 4), "a"), (Interval (5, 7), "b")]) (Timeline [(Interval (2, 6), "a"), (Interval (7, 9), "b")])
