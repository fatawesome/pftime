{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module : Timeline
-- Maintainer : fatawesomeee@gmail.com
-- = Timeline
--
-- The @'Timeline' t p@ represents the event set where 't' stands for time and 'p' for payload.
-----------------------------------------------------------------------------
module Timeline where

import Prelude hiding (take, takeWhile)

import           Interval
import           OverlappingTimeline
import Data.Maybe (mapMaybe)
import Data.Foldable (asum)

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> instance (Ord t, Arbitrary t) => Arbitrary (Interval t) where arbitrary = mkInterval <$> arbitrary
-- >>> instance (Arbitrary e, Ord t, Arbitrary t) => Arbitrary (Timeline t e) where arbitrary = fromListWith const <$> arbitrary
-- $setup

-----------------------------------------------------------------------------
-- * Timeline type

-- | Timeline
--
-- prop> isAscending t
-- prop> not (haveConflicts (toList t))
newtype Timeline t p = Timeline
  { getTimeline :: [(Interval t, p)] -- ^ Sorted list of intervals.
  } deriving (Show)


-----------------------------------------------------------------------------
-- * Construction

-- | /O(n^2)/. Construct Timeline from Overlapping Timeline with given payload conflicts resolver
-- 
fromOverlappingTimeline
  :: Ord t
  => (p -> p -> p)           -- ^ payload conflicts resolver
  -> OverlappingTimeline t p -- ^ input timeline with conflicts
  -> Timeline t p            -- ^ timeline without conflicts
fromOverlappingTimeline mergePayload (OverlappingTimeline xs) = resolveConflicts xs
  where
    resolveConflicts []     = empty
    resolveConflicts (t:ts) = foldr (insert mergePayload) (Timeline [t]) ts

-- | /O(n^2)/. Construct timeline from list of intervals with given payload conflicts resolver
fromListWith
  :: Ord t
  => (p -> p -> p)     -- ^ payload conflicts resolver
  -> [(Interval t, p)] -- ^ list of intervals from which to create a Timeline
  -> Timeline t p      -- ^ new Timeline
fromListWith f lst = fromOverlappingTimeline f (fromList lst)

-- | /O(1)/. Construct timeline from list without preserving timeline properties.
unsafeFromList :: [(Interval t, p)] -> Timeline t p
unsafeFromList = Timeline

-- | /O(1)/. Empty timeline.
--
-- > length (toList empty) == 0
empty :: Timeline t p
empty = Timeline []

-- | /O(1)/. Timeline with one event.
--
-- > length (toList (singleton (Interval (0, 1), 'a'))) == 1
singleton :: Ord t => (Interval t, p) -> Timeline t p
singleton event = Timeline [event]

-----------------------------------------------------------------------------
-- * Insertion

-- TODO: refactor in terms of `intersectIntervals` from Interval.hs
-- | Safely insert an element into the Timeline
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
  
-----------------------------------------------------------------------------
-- * Query

take
  :: Ord t
  => Int
  -> Timeline t e
  -> [(Interval t, e)]
take 0 _                 = []
take _ (Timeline [])     = []
take n (Timeline (x:xs)) = x : take (n-1) (Timeline xs)

takeWhile
  :: Ord t
  => ((Interval t, e) -> Bool)
  -> Timeline t e
  -> [(Interval t, e)]
takeWhile _ (Timeline []) = []
takeWhile f (Timeline (x:xs))
  | f x = x : takeWhile f (Timeline xs)
  | otherwise = []
  
-----------------------------------------------------------------------------
-- * Combine

union
  :: Ord t
  => (e -> e -> e)
  -> Timeline t e
  -> Timeline t e
  -> Timeline t e
union f (Timeline xs) (Timeline ys) = fromListWith f (xs <> ys)

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

-- TODO: optimization
-- Number of iterations for one interval can be reduced given the fact that
-- Timeline is ascending and non-overlapping (see isValid).
-- When x_2 (end of interval) is less than y_1 (beginning of the next interval to compare with)
-- all subsequent operations for current interval can be canceled.
--difference
--  :: Ord t
--  => Timeline t e
--  -> Timeline t e
--  -> Timeline t e
--difference (Timeline []) _ = Timeline []
--difference x (Timeline []) = x
--difference (Timeline xs) (Timeline excludes)
--  = unsafeFromList $ 
  -- [a] -> (a -> [a]) ->

-----------------------------------------------------------------------------
-- * Conversion

-- | Convert Timeline to list of Intervals
toList :: Timeline t e -> [(Interval t, e)]
toList = getTimeline

isAscending :: Ord a => [a] -> Bool
isAscending xs = and (zipWith (<) xs (drop 1 xs))

isValid :: Ord t => Timeline t e -> Bool
isValid = isAscending . map fst . toList

-----------------------------------------------------------------------------
-- * Helpers

findIntersection
  :: Ord t
  => Interval t         -- ^ timeline in which to search.
  -> [Interval t]       -- ^ interval to find intersection with.
  -> Maybe (Interval t) -- ^ intersection or Nothing.
findIntersection interval xs = asum (map (intersectIntervals interval) xs)

-- haveConflicts

subtractFromIntervalList
  :: Ord t
  => [(Interval t, e)] -- ^ List of intervals from which to subtract TODO: get rid of payloads here
  -> Interval t        -- ^ Interval to subtract.
  -> [(Interval t, e)] -- ^ Resulting list of intervals.
subtractFromIntervalList xs interval = concatMap (handleSubtractWithPayload interval) xs
  where
    handleSubtractWithPayload x y = map (, snd y) (subtractIntervalFlip x (fst y))
    subtractIntervalFlip x y = subtractInterval y x 

