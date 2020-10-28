{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
module Timeline where

import           Interval
import           OverlappingTimeline
import Data.List (find)
import Data.Maybe (isJust, catMaybes, mapMaybe)

-- | Timeline cannot have overlapping intervals.
--
-- prop> not (haveConflicts (toList t))
newtype Timeline t e = Timeline
  { getTimeline :: [(Interval t, e)]    -- ^ Sorted list of intervals.
  } deriving (Show)

-- TODO: refactor in terms of `intersect` from Interval.hs
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
  => Timeline t e          -- ^ timeline in which to search
  -> (Interval t, e)       -- ^ interval to find intersection with TODO: replace pair with only Interval
  -> Maybe (Interval t, e) -- ^ intersection or Nothing
findIntersection (Timeline xs) (interval, payload) 
  = case maybeIntersection of
    Just x -> Just (x, payload)
    Nothing -> Nothing
  where
    maybeIntersection = find (isJust . intersectIntervals interval) (map fst xs)
  
intersection
  :: Ord t
  => Timeline t e
  -> Timeline t e
  -> [(Interval t, e)]
intersection (Timeline xs) y 
  = mapMaybe (findIntersection y) xs   
  
--difference
--  :: Ord t
--  => Timeline t e
--  -> Timeline t e
--  -> [(Interval t, e)]
--difference (Timeline []) _ = []
--difference (Timeline xs) (Timeline []) = xs
--difference (Timeline xs) (Timeline excludes) = _

-- | Create Timeline without conflicts from Overlapping Timleine
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

