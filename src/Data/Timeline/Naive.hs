{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
-----------------------------------------------------------------------------
-- |
-- Module : Timeline
-- Maintainer : fatawesomeee@gmail.com
-- = Timeline
--
-- The @'Timeline' t p@ represents the event set where 't' stands for time and 'p' for payload.
-----------------------------------------------------------------------------
module Data.Timeline.Naive where

import           Data.Coerce
import           Prelude                   hiding (drop, dropWhile, filter,
                                            null, subtract, take, takeWhile)
import qualified Prelude

import           Data.Foldable             (asum)
import           Data.Maybe                (mapMaybe)

import           Data.Timeline.Event       as Event
import           Data.Timeline.Interval    as Interval
import           Data.Timeline.Overlapping

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Prelude hiding (take, takeWhile, subtract, null, filter, drop, dropWhile)
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

instance Ord t => Semigroup (Timeline t p) where
  (<>) = union (\_old new -> new)

instance Ord t => Monoid (Timeline t p) where
  mempty = empty

-----------------------------------------------------------------------------
-- * Construction

-- | \( O(n^2) \). Construct Timeline from Overlapping Timeline with given payload conflicts resolver
--
-- prop> fromOverlappingTimeline (++) overlapping == Timeline [Event (Interval (0,1)) "a", Event (Interval (1,2)) "ab", Event (Interval (2,3)) "b"]
fromOverlappingTimeline
  :: Ord t
  => (p -> p -> p)           -- ^ payload conflicts resolver
  -> OverlappingTimeline t p -- ^ input timeline with conflicts
  -> Timeline t p            -- ^ timeline without conflicts
fromOverlappingTimeline _ (OverlappingTimeline []) = empty
fromOverlappingTimeline f (OverlappingTimeline (x:xs)) = foldr (insert f) (Timeline [x]) xs

-- | \( O(n^2) \). Construct timeline from list of intervals with given payload conflicts resolver
--
-- prop> fromListWith (++) [event_0_2_a, event_1_3_b] == Timeline [Event (Interval (0,1)) "a", Event (Interval (1,2)) "ab", Event (Interval (2,3)) "b"]
fromListWith
  :: Ord t
  => (p -> p -> p)     -- ^ payload conflicts resolver
  -> [Event t p] -- ^ list of intervals from which to create a Timeline
  -> Timeline t p      -- ^ new Timeline
fromListWith f lst = fromOverlappingTimeline f (fromList lst)

-- | \( O(1) \). Construct timeline from list without preserving timeline properties.
--
-- prop> unsafeFromList [event_0_2_a, event_1_3_b] == Timeline [event_0_2_a, event_1_3_b]
unsafeFromList :: [Event t p] -> Timeline t p
unsafeFromList = Timeline

-- | \( O(1) \). Empty timeline.
--
-- >>> length (toList empty)
-- 0
empty :: Timeline t p
empty = Timeline []

-- | \( O(1) \). Timeline with one event.
--
-- >>> length (toList (singleton (Event (Interval (0, 1)) 'a')))
-- 1
singleton :: Event t p -> Timeline t p
singleton event = Timeline [event]

-----------------------------------------------------------------------------
-- * Insertion

-- TODO: refactor in terms of `intersectIntervals` from Interval.hs
-- | \( O(n) \). Safely insert an element into the Timeline
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
insert _ event (Timeline []) = singleton event
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

-- | \( O(n) \). Delete all entries for the given range from timeline.
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
--
-- >>> toString $ delete (Interval (5, 7)) (mkPictoralTimeline "xxx yyy")
-- "xxx y"
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
  | l >= rx = Timeline (x : toList (delete i (Timeline xs)))

  -- Case 2:
  -- xxx
  --  xxx
  | r <= rx = Timeline $ insertPayload diff px ++ xs

  -- Case 3:
  --  xxx
  -- xxx
  | r > rx = Timeline (insertPayload diff px ++ toList (delete (Interval (rx, r)) (Timeline xs)))

  | otherwise = timeline
  where
    diff = subtract ix i
    insertPayload is p = map (`Event` p) is


-- | Update timeline with event.
-- If given event intersects with those already in timeline, replaces them.
-- If not, simply inserts new event.
--
-- Basically, /update/ is /insert (\_, x -> x)/.
--
-- >>> let event = Event (Interval (0, 1)) 'x'
-- >>> toString $ update event (mkPictoralTimeline "")
-- "x"
--
-- >>> let event = Event (Interval (1, 2)) 'y'
-- >>> toString $ update event (mkPictoralTimeline "xxx")
-- "xyx"
--
-- >>> let event = Event (Interval (3, 4)) 'y'
-- >>> toString $ update event (mkPictoralTimeline "xxx")
-- "xxxy"
--
-- >>> let event = Event (Interval (2, 4)) 'y'
-- >>> toString $ update event (mkPictoralTimeline "xxx")
-- "xxyy"
update
  :: Ord t
  => Event t p
  -> Timeline t p
  -> Timeline t p
update = insert (\_ x -> x)

-----------------------------------------------------------------------------
-- * Size

-- | Is the timeline empty?
--
-- prop> null empty == True
-- prop> null (singleton (Event (Interval (0,1)) 'a')) == False
null :: Timeline t p -> Bool
null t = size t == 0

-- | The number of event in the timeline.
--
-- prop> size empty == 0
-- prop> size (singleton (Event (Interval (0,1)) 'a')) == 1
-- prop> size (mkPictoralTimeline "xyz") == 3
size :: Timeline t p -> Int
size = length . getTimeline

-----------------------------------------------------------------------------
-- * Query

-- | \( O(n) \). Get first `n` events from timeline.
--
-- prop> take 1 (mkPictoralTimeline "") == empty
-- prop> take 2 (mkPictoralTimeline "x") == singleton (Event (Interval (0,1)) 'x')
-- prop> take 2 (mkPictoralTimeline "xy") == mkPictoralTimeline "xy"
-- prop> take 2 (mkPictoralTimeline "xyz") == mkPictoralTimeline "xy"
take
  :: Ord t
  => Int
  -> Timeline t p
  -> Timeline t p
take 0 _                 = empty
take _ (Timeline [])     = empty
take n (Timeline (x:xs)) = Timeline (x : getTimeline (take (n-1) (Timeline xs)))

-- | \( O(n) \). Get events while they satisfy given condition.
--
-- prop> takeWhile (\x -> payload x == 'x') (mkPictoralTimeline "xxx y xx") == mkPictoralTimeline "xxx"
-- prop> takeWhile (\x -> payload x == 'x') (mkPictoralTimeline "y xx") == empty
takeWhile
  :: Ord t
  => (Event t p -> Bool)
  -> Timeline t p
  -> Timeline t p
takeWhile _ (Timeline []) = empty
takeWhile f (Timeline (x:xs))
  | f x = Timeline (x : getTimeline (takeWhile f (Timeline xs)))
  | otherwise = empty

--flatMap
--  :: (Event t a -> Timeline t b)
--  -> Timeline t a
--  -> Timeline t b
--flatMap f timeline = _

-- | \( O(n) \). Filter all events that satisfy the predicate.
--
-- >>> let t = "x y x z" :: PictoralTimeline
-- >>> filter (\x -> payload x == 'x') t
-- x   x
filter
  :: Ord t
  => (Event t p -> Bool)
  -> Timeline t p
  -> Timeline t p
filter _ (Timeline []) = empty
filter f (Timeline (x:xs))
  | f x = Timeline (x : getTimeline xs')
  | otherwise = xs'
  where xs' = filter f (Timeline xs)

-- | \( O(n) \). Cut out a window from the timeline.
--
-- >>> let t = "xxxxxx" :: PictoralTimeline
-- >>> window (Interval (0, 1)) t
-- x
--
-- >>> let t = "xxxxxx" :: PictoralTimeline
-- >>> window (Interval (1, 3)) t
--  xx
--
-- >>> let t = " xx" :: PictoralTimeline
-- >>> window (Interval (0, 2)) t
--  x
--
-- >>> let t = " xx" :: PictoralTimeline
-- >>> window (Interval (0, 2)) t
--  x
--
-- >>> let t = "xxx xxx" :: PictoralTimeline
-- >>> window (Interval (2, 4)) t
--   x
--
-- >>> let t = "xxx xxx" :: PictoralTimeline
-- >>> window (Interval (2, 5)) t
--   x x
--
-- >>> let t = "xx xx xx" :: PictoralTimeline
-- >>> window (Interval (1, 7)) t
--  x xx x
window
  :: Ord t
  => Interval t
  -> Timeline t p
  -> Timeline t p
window _ (Timeline []) = empty
window i@(Interval (l, r)) (Timeline ((Event (Interval (lx, rx)) px) : xs))
  -- Case 1:
  --    iii
  -- xxx
  | l >= rx = window i (Timeline xs)

  -- Case 2:
  -- iii
  --  xxx
  | r <= rx = singleton (Event (Interval (max l lx, r)) px)

  -- Case 3:
  --  iii
  -- xxx
  | r > rx
    = Timeline (Event (Interval (max l lx, rx)) px : getTimeline (window (Interval (rx, r)) (Timeline xs)))

  | otherwise = empty

-- | \( O(n) \). Return suffix of timeline after the first `n` elements, or empty timeline if n > size timeline.
--
-- >>> let t = "xxx" :: PictoralTimeline
-- >>> drop 0 t
-- xxx
--
-- >>> let t = "xxx yyy" :: PictoralTimeline
-- >>> drop 3 t == empty
-- True
--
-- >>> let t = "xxx yyy" :: PictoralTimeline
-- >>> drop 2 t == empty
-- True
--
-- >>> let t = "xxx yyy zzz" :: PictoralTimeline
-- >>> drop 1 t
--     yyy zzz
drop
  :: Ord t
  => Int
  -> Timeline t p
  -> Timeline t p
drop _ (Timeline []) = empty
drop n timeline@(Timeline (_:xs))
  | n > 0     = drop (n-1) (Timeline xs)
  | otherwise = timeline


-- | Return suffix after dropping events which satisfy the predicate.
--
-- >>> let t = "yyy" :: PictoralTimeline
-- >>> dropWhile (\e -> payload e == 'x') t
-- yyy
--
-- >>> let t = "x yyy" :: PictoralTimeline
-- >>> dropWhile (\e -> payload e == 'x') t
--   yyy
--
-- >>> let t = "x x yyy" :: PictoralTimeline
-- >>> dropWhile (\e -> payload e == 'x') t
--     yyy
--
-- >>> let t = "z xxx yyy" :: PictoralTimeline
-- >>> dropWhile (\e -> payload e == 'x') t
-- z xxx yyy
dropWhile
  :: Ord t
  => (Event t p -> Bool)
  -> Timeline t p
  -> Timeline t p
dropWhile _ (Timeline []) = empty
dropWhile f t@(Timeline (x:xs))
  | f x = Timeline (getTimeline (dropWhile f (Timeline xs)))
  | otherwise = t

-- | Drop everything until given point in time.
--
-- >>> let t = "xxx xxx xxx" :: PictoralTimeline
-- >>> dropBefore 4 t
--     xxx xxx
--
-- >>> let t = "xxx" :: PictoralTimeline
-- >>> dropBefore 1 t
--  xx
--
-- >>> let t = "  xxx" :: PictoralTimeline
-- >>> dropBefore 1 t
--   xxx
dropBefore
  :: Ord t
  => t
  -> Timeline t p
  -> Timeline t p
dropBefore _ (Timeline []) = empty
dropBefore t timeline@(Timeline (Event (Interval (l, r)) p : xs))
  | t <= l = timeline
  | t >= r = dropBefore t (Timeline xs)
  | otherwise = Timeline (Event (Interval (t, r)) p : xs )

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

-- | \( O(n+m) \). Returns timeline union of two timelines. For example,
--
-- >>> let t1 = "xxx"     :: PictoralTimeline
-- >>> let t2 = "    yyy" :: PictoralTimeline
-- >>> unionBy (\a b -> b) t1 t2
-- xxxyyy
--
-- >>> let t1 = "xxx" :: PictoralTimeline
-- >>> let t2 = "yyy" :: PictoralTimeline
-- >>> unionBy (\a b -> b) t1 t2
-- yyy
--
-- >>> let t1 = "xxx" :: PictoralTimeline
-- >>> let t2 = " yyy" :: PictoralTimeline
-- >>> unionBy (\a b -> b) t1 t2
-- xyyy
--
-- >>> let t1 = " xxx" :: PictoralTimeline
-- >>> let t2 = "yyy" :: PictoralTimeline
-- >>> unionBy (\a b -> b) t1 t2
-- yyyx
--unionBy
--  :: Ord t
--  => (p -> p -> p)
--  -> Timeline t p
--  -> Timeline t p
--  -> Timeline t p
--unionBy _ t (Timeline []) = t
--unionBy _ (Timeline []) t = t
--unionBy f t1@(Timeline x:xs) t2@(Timeline y:ys)
--  |


-- TODO: optimization
-- As we know, Timeline is ascending and has no overlaps (see isValid).
-- So, after each iteration processed interval can be dropped,
-- thus decreasing number of operations to be performed in the next iteration.

-- | Find intersection of the first timeline with the second.
--
-- >>> toString $ (mkPictoralTimeline "xxx") `Timeline.intersect` (mkPictoralTimeline " yyy")
-- " xx"
--
-- >>> toString $ (mkPictoralTimeline "xxx") `Timeline.intersect` (mkPictoralTimeline "    yyy")
-- ""
--
-- >>> toString $ (mkPictoralTimeline "xxx yyy") `Timeline.intersect` (mkPictoralTimeline "  zzz")
-- "  x y"
intersect
  :: Ord t
  => Timeline t p
  -> Timeline t p
  -> Timeline t p
intersect (Timeline xs) (Timeline ys)
  = unsafeFromList $ mapMaybe (handleIntersectionWithPayload ys) xs
  where
    handleIntersectionWithPayload t (Event i p)
      = case findIntersectionFlip (map interval t) i of
        Just x  -> Just $ Event x p
        Nothing -> Nothing
    findIntersectionFlip x y = findIntersection y x

-- TODO: optimization
-- Number of iterations for one interval can be reduced given the fact that
-- Timeline is ascending and non-overlapping (see isValid).
-- When x_2 (end of interval) is less than y_1 (beginning of the next interval to compare with)
-- all subsequent operations for current interval can be canceled.

-- | Find difference of firs timeline from second.
--
-- >>> toString $ (mkPictoralTimeline "xxx") `difference` (mkPictoralTimeline "yyy")
-- ""
--
-- >>> toString $ (mkPictoralTimeline "xxx") `difference` (mkPictoralTimeline "   yyy")
-- "xxx"
--
-- >>> toString $ (mkPictoralTimeline "xxx") `difference` (mkPictoralTimeline "  yyy")
-- "xx"
--
-- >>> toString $ (mkPictoralTimeline "xxx yyy") `difference` (mkPictoralTimeline "xx   yy")
-- "  x y"
difference
  :: Ord t
  => Timeline t p
  -> Timeline t p
  -> Timeline t p
difference x (Timeline []) = x
difference x (Timeline ((Event iy _):ys)) = delete iy x `difference` Timeline ys

-----------------------------------------------------------------------------
-- * Conversion

-- | Convert Timeline to list of Intervals
toList :: Timeline t p -> [Event t p]
toList = getTimeline

isAscending :: Ord a => [a] -> Bool
isAscending xs = and (zipWith (<) xs (Prelude.drop 1 xs))

isValid :: Ord t => Timeline t p -> Bool
isValid = isAscending . map interval . toList

-----------------------------------------------------------------------------
-- * Helpers

findIntersection
  :: Ord t
  => Interval t         -- ^ interval to find intersection with.
  -> [Interval t]       -- ^ intervals in which to search.
  -> Maybe (Interval t) -- ^ intersection or Nothing.
findIntersection i xs = asum (map (Interval.intersect i) xs)

withReference
  :: (Ord a, Ord r, Num r, Coercible a r)
  => Timeline a p
  -> Timeline r p
  -> Timeline a p
withReference = error "not implemented"
