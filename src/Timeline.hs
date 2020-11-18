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

import Prelude hiding (take, takeWhile, subtract)
import Event

import           Interval
import           OverlappingTimeline
import Data.Maybe (mapMaybe)
import Data.Foldable (asum)

-- $setup
-- >>> import Prelude hiding (take, takeWhile)
-- >>> import PictoralTimeline
-- >>> let event_0_2_a = Event (Interval (0, 2)) "a"
-- >>> let event_1_3_b = Event (Interval (1, 3)) "b"
-- >>> let overlapping = OverlappingTimeline.fromList [event_0_2_a, event_1_3_b]
-- $setup

-----------------------------------------------------------------------------
-- * Timeline type

-- | Timeline
--
-- > isAscending t
-- > not (haveConflicts (toList t))
newtype Timeline t p = Timeline
  { getTimeline :: [Event t p] -- ^ Sorted list of intervals.
  } deriving (Show, Eq)


-----------------------------------------------------------------------------
-- * Construction

-- | /O(n^2)/. Construct Timeline from Overlapping Timeline with given payload conflicts resolver
-- 
-- prop> fromOverlappingTimeline (++) overlapping == Timeline [Event (Interval (0,1)) "a", Event (Interval (1,2)) "ab", Event (Interval (2,3)) "b"]
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
-- 
-- prop> fromListWith (++) [event_0_2_a, event_1_3_b] == Timeline [Event (Interval (0,1)) "a", Event (Interval (1,2)) "ab", Event (Interval (2,3)) "b"]
fromListWith
  :: Ord t
  => (p -> p -> p)     -- ^ payload conflicts resolver
  -> [Event t p] -- ^ list of intervals from which to create a Timeline
  -> Timeline t p      -- ^ new Timeline
fromListWith f lst = fromOverlappingTimeline f (fromList lst)

-- | /O(1)/. Construct timeline from list without preserving timeline properties.
-- 
-- prop> unsafeFromList [event_0_2_a, event_1_3_b] == Timeline [event_0_2_a, event_1_3_b]
unsafeFromList :: [Event t p] -> Timeline t p
unsafeFromList = Timeline

-- | /O(1)/. Empty timeline.
--
-- >>> length (toList empty)
-- 0
empty :: Timeline t p
empty = Timeline []

-- | /O(1)/. Timeline with one event.
--
-- >>> length (toList (singleton (Event (Interval (0, 1)) 'a')))
-- 1
singleton :: Event t p -> Timeline t p
singleton event = Timeline [event]

-----------------------------------------------------------------------------
-- * Insertion

-- TODO: refactor in terms of `intersectIntervals` from Interval.hs
-- | Safely insert an element into the Timeline
-- 
-- Case 1: 
-- 
-- >>> toString $ insert (\_ y -> y) (Event (Interval (0,3)) 'y') (mkPictoralTimeline "   xxx")
-- "yyyxxx"
-- 
-- Case 2:
--  
-- >>> toString $ insert (\_ y -> y) (Event (Interval (0,3)) 'y') (mkPictoralTimeline " xxx")
-- "yyyx"
-- 
-- Case 3:
--  
-- >>> toString $ insert (\_ y -> y) (Event (Interval (0,4)) 'y') (mkPictoralTimeline " xxx")
-- "yyyy"
-- 
-- Case 4:
--  
-- >>> toString $ insert (\_ y -> y) (Event (Interval (3,6)) 'y') (mkPictoralTimeline "xxx")
-- "xxxyyy"
-- 
-- Case 5:
--  
-- >>> toString $ insert (\_ y -> y) (Event (Interval (1,4)) 'y') (mkPictoralTimeline "xxx")
-- "xyyy"
--
-- Case 6:
--    
-- >>> toString $ insert (\_ y -> y) (Event (Interval (0,4)) 'y') (mkPictoralTimeline "xxx")
-- "yyyy"
-- 
-- Case 7:
--  
-- >>> toString $ insert (\_ y -> y) (Event (Interval (0,3)) 'y') (mkPictoralTimeline "xxx")
-- "yyy"
-- 
-- Case 8:
--  
-- >>> toString $ insert (\_ y -> y) (Event (Interval (0,5)) 'y') (mkPictoralTimeline " xxx")
-- "yyyyy"
-- 
-- Case 9: 
-- 
-- >>> toString $ insert (\_ y -> y) (Event (Interval (1,4)) 'y') (mkPictoralTimeline "xxxxx")
-- "xyyyx"
-- 
-- Case 10:
--  
-- >>> toString $ insert (\_ y -> y) (Event (Interval (0,3)) 'y') (mkPictoralTimeline "xxxx")
-- "yyyx"
-- 
-- Case 11:
--  
-- >>> toString $ insert (\_ y -> y) (Event (Interval (1,4)) 'y') (mkPictoralTimeline "xxxx")
-- "xyyy"
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
        , Event (Interval (yleft, xright)) (f pX pY)
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
        , Event (Interval (xleft, xright)) (f pX pY)
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

  | otherwise = timeline

-----------------------------------------------------------------------------
-- * Delete/Update

-- | Delete all entries for the given range from timeline.
--
-- >>> toString $ delete (Interval (0, 1)) (mkPictoralTimeline " xx")
-- " xx"
--
-- >>> toString $ delete (Interval (2, 3)) (mkPictoralTimeline "xx")
-- "xx"
--
-- >>> toString $ delete (Interval (2, 5)) (mkPictoralTimeline "xxx")
-- "xx"
--
-- >>> toString $ delete (Interval (0, 2)) (mkPictoralTimeline " xxx")
-- "  xx"
--
-- >>> toString $ delete (Interval (1, 2)) (mkPictoralTimeline "xxx")
-- "x x"
--
-- >>> toString $ delete (Interval (2, 5)) (mkPictoralTimeline "xxx yyy")
-- "xx   yy"
--
-- >>> toString $ delete (Interval (0, 3)) (mkPictoralTimeline " x")
-- ""
delete
  :: Ord t
  => Interval t
  -> Timeline t p
  -> Timeline t p
delete _ (Timeline []) = Timeline []
delete i@(Interval (l, r)) timeline@(Timeline (x@(Event ix@(Interval (_, rx)) px):xs))
  -- Case 1:
  --    xxx
  -- xxx
  | l >= rx = Timeline (x : getTimeline (delete i (Timeline xs)))

  -- Case 2:
  -- xxx
  --  xxx
  | r <= rx = Timeline $ insertPayload difference px ++ xs

  -- Case 3:
  --  xxx
  -- xxx
  | r > rx = Timeline (insertPayload difference px ++ getTimeline (delete (Interval (rx, r)) (Timeline xs)))

  | otherwise = timeline
  where
    difference = subtract ix i
    insertPayload is p = map (`Event` p) is
      
-----------------------------------------------------------------------------
-- * Query

-- | Get first `n` events from timeline.
--
-- prop> take 1 (mkPictoralTimeline "") == []
-- prop> take 2 (mkPictoralTimeline "x") == [Event (Interval (0,1)) 'x']
-- prop> take 2 (mkPictoralTimeline "xy") == getTimeline (mkPictoralTimeline "xy")
-- prop> take 2 (mkPictoralTimeline "xyz") == [Event (Interval (0,1)) 'x', Event (Interval (1,2)) 'y']
take
  :: Ord t
  => Int
  -> Timeline t p
  -> [Event t p]
take 0 _                 = []
take _ (Timeline [])     = []
take n (Timeline (x:xs)) = x : take (n-1) (Timeline xs)

-- | Get events while they satisfy given condition.
--
-- prop> takeWhile (\x -> payload x == 'x') (mkPictoralTimeline "xxx y xx") == [Event (Interval (0, 3)) 'x']
-- prop> takeWhile (\x -> payload x == 'x') (mkPictoralTimeline "y xx") == []
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

-- |
-- 
-- >>> toString $ union (\_ y -> y) (mkPictoralTimeline "xxx") (mkPictoralTimeline "")
-- "xxx"
--
-- >>> toString $ union (\_ y -> y) (mkPictoralTimeline "xxx") (mkPictoralTimeline "   yyy")
-- "xxxyyy"
--
-- >>> toString $ union (\_ y -> y) (mkPictoralTimeline "xx") (mkPictoralTimeline "   yy")
-- "xx yy"
--
-- >>> toString $ union (\_ y -> y) (mkPictoralTimeline " x y z") (mkPictoralTimeline "x y z")
-- "xxyyzz"
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
    subtractIntervalFlip x y = subtract y x

