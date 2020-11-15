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
import Event

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
  { getTimeline :: [Event t p] -- ^ Sorted list of intervals.
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
fromOverlappingTimeline f (OverlappingTimeline xs) = resolveConflicts xs
  where
    resolveConflicts []     = empty
    resolveConflicts (t:ts) = foldr (insert f) (Timeline [t]) ts

-- | /O(n^2)/. Construct timeline from list of intervals with given payload conflicts resolver
fromListWith
  :: Ord t
  => (p -> p -> p)     -- ^ payload conflicts resolver
  -> [Event t p] -- ^ list of intervals from which to create a Timeline
  -> Timeline t p      -- ^ new Timeline
fromListWith f lst = fromOverlappingTimeline f (fromList lst)

-- | /O(1)/. Construct timeline from list without preserving timeline properties.
unsafeFromList :: [Event t p] -> Timeline t p
unsafeFromList = Timeline

-- | /O(1)/. Empty timeline.
--
-- > length (toList empty) == 0
empty :: Timeline t p
empty = Timeline []

-- | /O(1)/. Timeline with one event.
--
-- > length (toList (singleton (Interval (0, 1), 'a'))) == 1
singleton :: Event t p -> Timeline t p
singleton event = Timeline [event]

-----------------------------------------------------------------------------
-- * Insertion

-- TODO: refactor in terms of `intersectIntervals` from Interval.hs
-- | Safely insert an element into the Timeline
insert
  :: Ord t
  => (p -> p -> p)
  -> Event t p
  -> Timeline t p
  -> Timeline t p
insert _ event (Timeline []) = Timeline [event]
insert 
  f 
  event@(Event (Interval (yleft, yright)) pY) 
  timeline@(Timeline (x@(Event (Interval (xleft, xright)) pX) : xs))
 
  -- 1
  --    xxx
  -- yyy
  | yright <= xleft
    = Timeline (event : x : xs)
    
  -- 2
  --  xxx
  -- yyy
  | yleft < xleft && yright < xright 
    = Timeline (
        [ Event (Interval (yleft, xleft)) pY
        , Event (Interval (xleft, yright)) (f pX pY)
        , Event (Interval (yright, xright)) pX
        ] <> xs
      )
      
  -- 3
  --  xxx
  -- yyyy
  | yleft < xleft && yright == xright 
    = Timeline (
        [ Event (Interval (yleft, xleft)) pY
        , Event (Interval (xleft, yright)) (f pX pY)
        ] <> xs
      )
  
  -- 4
  -- xxx
  --    yyy 
  | yleft >= xright
    = Timeline (x : getTimeline (insert f event (Timeline xs)))
    
  -- 5
  -- xxx
  --  yyy
  | yleft > xleft && yright > xright 
    = Timeline (
        [ Event (Interval (xleft, yleft)) pX
        , Event (Interval (xleft, yright)) (f pX pY)
        , Event (Interval (xright, yright)) pY
        ] <> xs
      )
     
  -- 6 
  -- xxx
  -- yyyy
  | yleft == xleft && yright > xright
    = Timeline (
        [ Event (Interval (yleft, xright)) (f pX pY)
        , Event (Interval (xright, yright)) pY
        ] <> xs
      )
  
  -- 7
  -- xxx
  -- yyy
  | yleft == xleft && yright == xright
    = Timeline $ Event (Interval (yleft, yright)) (f pX pY) : xs
      
  -- 8
  --  xxx
  -- yyyyy
  | yleft < xleft && yright > xright 
    = Timeline (
        [ Event (Interval (yleft, xleft)) pY
        , Event (Interval (xleft, yright)) (f pX pY)
        ] <> getTimeline (insert f (Event (Interval (xright, yright)) pY) (Timeline xs))
      )
  
  -- 9
  -- xxxxx
  --  yyy
  | yleft > xleft && yright < xright
    = Timeline (
        [ Event (Interval (xleft, yleft)) pX
        , Event (Interval (yleft, yright)) (f pX pY)
        , Event (Interval (yright, xright)) pX
        ] <> xs
      ) 
  
  -- 10
  -- xxxx
  -- yyy
  | yleft == xleft && yright < xright
    = Timeline (
        [ Event (Interval (yleft, yright)) (f pX pY)
        , Event (Interval (yright, xright)) pX
        ] <> xs
      )
      
  -- 11
  -- xxxx
  --  yyy
  | yleft > xleft && yright == xright
    = Timeline (
        [ Event (Interval (xleft, yleft)) pX
        , Event (Interval (yleft, yright)) (f pX pY)
        ] <> xs
      )
      
-----------------------------------------------------------------------------
-- * Query

take
  :: Ord t
  => Int
  -> Timeline t p
  -> [Event t p]
take 0 _                 = []
take _ (Timeline [])     = []
take n (Timeline (x:xs)) = x : take (n-1) (Timeline xs)

takeWhile
  :: Ord t
  => (Event t p -> Bool)
  -> Timeline t p
  -> [Event t p]
takeWhile _ (Timeline []) = []
takeWhile f (Timeline (x:xs))
  | f x = x : takeWhile f (Timeline xs)
  | otherwise = []
  
-----------------------------------------------------------------------------
-- * Combine

union
  :: Ord t
  => (p -> p -> p)
  -> Timeline t p
  -> Timeline t p
  -> Timeline t p
union f (Timeline xs) (Timeline ys) = fromListWith f (xs <> ys)


-- TODO: optimization
-- As we know, Timeline is ascending and has no overlaps (see isValid).
-- So, after each iteration processed interval can be dropped,
-- thus decreasing number of operations to be performed in the next iteration.
intersection
  :: Ord t
  => Timeline t p
  -> Timeline t p
  -> Timeline t p
intersection (Timeline xs) (Timeline ys)
  = unsafeFromList $ mapMaybe (handleIntersectionWithPayload ys) xs
  where
    handleIntersectionWithPayload t (Event i p)
      = case findIntersectionFlip (map interval t) i of
        Just x -> Just $ Event x p 
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
toList :: Timeline t p -> [Event t p]
toList = getTimeline

isAscending :: Ord a => [a] -> Bool
isAscending xs = and (zipWith (<) xs (drop 1 xs))

isValid :: Ord t => Timeline t p -> Bool
isValid = isAscending . map interval . toList

-----------------------------------------------------------------------------
-- * Helpers

findIntersection
  :: Ord t
  => Interval t         -- ^ timeline in which to search.
  -> [Interval t]       -- ^ interval to find intersection with.
  -> Maybe (Interval t) -- ^ intersection or Nothing.
findIntersection i xs = asum (map (intersectIntervals i) xs)

-- haveConflicts

subtractFromIntervalList
  :: Ord t
  => [(Interval t, e)] -- ^ List of intervals from which to subtract TODO: get rid of payloads here
  -> Interval t        -- ^ Interval to subtract.
  -> [(Interval t, e)] -- ^ Resulting list of intervals.
subtractFromIntervalList xs i = concatMap (handleSubtractWithPayload i) xs
  where
    handleSubtractWithPayload x y = map (, snd y) (subtractIntervalFlip x (fst y))
    subtractIntervalFlip x y = subtractInterval y x 

