{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE TupleSections #-}
module Timeline where

import           Interval
import           OverlappingTimeline
import Data.Maybe (mapMaybe)
import Data.Foldable (asum)

-- | Timeline cannot have overlapping intervals.
--
-- prop> not (haveConflicts (toList t))
newtype Timeline t e = Timeline
  { getTimeline :: [(Interval t, e)]    -- ^ Sorted list of intervals.
  } deriving (Show)

-- TODO: refactor in terms of `intersectIntervals` from Interval.hs
insert
  :: Ord t
  => (e -> e -> e)
  -> (Interval t, e)
  -> Timeline t e
  -> Timeline t e
insert _ el (Timeline []) = Timeline [el]
insert mergePayload el@(Interval (left, right), e) timeline@(Timeline (x@(Interval (xleft, xright), eX) : xs))
  | right <= xleft
    = Timeline (el:x:xs)
  | left < xleft && right < xright -- && right > xleft
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
  | right == xright && left > xleft
    = Timeline (
      [ (Interval (xleft, left), eX)
      , (Interval (left, right), mergePayload eX e)
      ] <> xs
    )
  | otherwise = timeline

mergeTimeline
  :: Ord t
  => (e -> e -> e)
  -> Timeline t e
  -> Timeline t e
  -> Timeline t e
mergeTimeline f (Timeline xs) (Timeline ys) = fromListWith f (xs <> ys)

take' 
  :: Ord t
  => Int
  -> Timeline t e
  -> [(Interval t, e)]
take' 0 _                 = []
take' _ (Timeline [])     = []
take' n (Timeline (x:xs)) = x : take' (n-1) (Timeline xs)

takeWhile' 
  :: Ord t
  => ((Interval t, e) -> Bool)
  -> Timeline t e
  -> [(Interval t, e)]
takeWhile' _ (Timeline []) = []
takeWhile' f (Timeline (x:xs))
  | f x = x : takeWhile' f (Timeline xs)
  | otherwise = []
    
findIntersection
  :: Ord t
  => Interval t         -- ^ timeline in which to search.
  -> [Interval t]       -- ^ interval to find intersection with.
  -> Maybe (Interval t) -- ^ intersection or Nothing.
findIntersection interval xs = asum (map (intersectIntervals interval) xs)
  
-- TODO: optimization
-- As we know, Timeline is ascending and has no overlaps (see isValid).
-- So, after each iteration processed interval can be dropped, 
-- thus decreasing number of operations to be performed in the next iteration. 
intersection
  :: Ord t
  => Timeline t e
  -> Timeline t e
  -> Timeline t e
intersection (Timeline xs) (Timeline ys) 
  = unsafeFromList $ mapMaybe (handleIntersectionWithPayload ys) xs
  where
    handleIntersectionWithPayload timeline interval  
      = case findIntersectionFlip (map fst timeline) (fst interval) of
        Just x -> Just (x, snd interval)
        Nothing -> Nothing
    findIntersectionFlip x y = findIntersection y x     
  
subtractFromInterval
  :: Ord t
  => Interval t   -- ^ Interval to subtract from.
  -> [Interval t] -- ^ List of intervals which will be subtracted.
  -> [Interval t] -- ^ Resulting list of intervals.
subtractFromInterval interval = concatMap (subtractInterval interval) 

-- TODO: optimization
-- Number of iterations for one interval can be reduced given the fact that
-- Timeline is ascending and non-overlapping (see isValid).
-- When x2 (end of interval) is less than y1 (beginning of the next interval to compare with)
-- all subsequent operations for current interval can be canceled.
difference
  :: Ord t
  => Timeline t e
  -> Timeline t e
  -> Timeline t e
difference (Timeline []) _ = Timeline []
difference x (Timeline []) = x
difference (Timeline xs) (Timeline excludes) 
  = unsafeFromList $ concatMap (handleSubtractWithPayload excludes) xs
  where 
    handleSubtractWithPayload timeline interval 
      = map (, snd interval) (subtractFromIntervalFlipped (map fst timeline) (fst interval))
    subtractFromIntervalFlipped x y = subtractFromInterval y x 

-- | Create Timeline without conflicts from Overlapping Timeline
-- | O((n^2)/2)
fromOverlappingTimeline
  :: Ord t
  => (e -> e -> e)           -- ^ merge payload
  -> OverlappingTimeline t e -- ^ input timeline with conflicts
  -> Timeline t e            -- ^ timeline without conflicts
fromOverlappingTimeline mergePayload (OverlappingTimeline xs) = resolveConflicts xs
  where
    resolveConflicts []     = emptyTimeline
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

unsafeFromList :: [(Interval t, e)] -> Timeline t e
unsafeFromList = Timeline

emptyTimeline :: Timeline t e
emptyTimeline = Timeline []

isAscending :: Ord a => [a] -> Bool
isAscending xs = and (zipWith (<) xs (drop 1 xs))

isValid :: Ord t => Timeline t e -> Bool
isValid = isAscending . map fst . toList

