{-# LANGUAGE DeriveFunctor #-}
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

import           Prelude                   hiding (drop, dropWhile, filter,
                                            null, reverse, subtract, take,
                                            takeWhile)
import qualified Prelude

import           Data.Foldable             (asum)
import           Data.Maybe                (mapMaybe)

import           Data.Timeline.Event       as Event
import           Data.Timeline.Interval    hiding (intersect, shiftWith)
import qualified Data.Timeline.Interval    as Interval
import           Data.Timeline.Overlapping as Overlapping
import           GHC.Base                  (join)


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Prelude hiding (take, takeWhile, subtract, null, filter, drop, dropWhile)
-- >>> import Data.Timeline.Pictoral
-- >>> let event_0_2_a = Event (Interval (0, 2)) "a"
-- >>> let event_1_3_b = Event (Interval (1, 3)) "b"
-- >>> let overlapping = Overlapping.fromList [event_0_2_a, event_1_3_b]
-- $setup

-----------------------------------------------------------------------------
-- * Timeline type

-- | Timeline
--
-- > isAscending t
-- > not (haveConflicts (toList t))
newtype Timeline t p = Timeline
  { getTimeline :: [Event t p] -- ^ Sorted list of intervals.
  } deriving (Show, Eq, Functor)

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
        , Event (Interval (yright, xright)) pX -- TODO: that is a bug. This interval should be checked for overlaps with the next one.
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
        , Event (Interval (xright, yright)) pY -- TODO: that is a bug. This interval should be checked for overlaps with the next one.
        ] <> xs
      )

  -- 6
  -- xxx
  -- yyyy
  | yleft == xleft && yright > xright
    = Timeline (
        [ Event (Interval (yleft, xright)) (f pX pY)
        , Event (Interval (xright, yright)) pY -- TODO: that is a bug. This interval should be checked for overlaps with the next one.
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
-- >>> delete (Interval (0, 1)) " xx" :: PictoralTimeline
--  xx
--
-- >>> delete (Interval (2, 3)) "xx" :: PictoralTimeline
-- xx
--
-- >>> delete (Interval (2, 5)) "xxx" :: PictoralTimeline
-- xx
--
-- >>> delete (Interval (0, 2)) " xxx" :: PictoralTimeline
--   xx
--
-- >>> delete (Interval (1, 2)) "xxx" :: PictoralTimeline
-- x x
--
-- >>> delete (Interval (2, 5)) "xxx yyy" :: PictoralTimeline
-- xx   yy
--
-- >>> delete (Interval (5, 7)) "xxx yyy" :: PictoralTimeline
-- xxx y
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
-- >>> dropWhile (\e -> getPayload e == 'x') t
-- yyy
--
-- >>> let t = "x yyy" :: PictoralTimeline
-- >>> dropWhile (\e -> getPayload e == 'x') t
--   yyy
--
-- >>> let t = "x x yyy" :: PictoralTimeline
-- >>> dropWhile (\e -> getPayload e == 'x') t
--     yyy
--
-- >>> let t = "z xxx yyy" :: PictoralTimeline
-- >>> dropWhile (\e -> getPayload e == 'x') t
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
-- >>> takeWhile (\x -> getPayload x == 'x') "xxx y xx" :: PictoralTimeline
-- xxx
--
-- >>> let t = "y xx" :: PictoralTimeline
-- >>> takeWhile (\x -> getPayload x == 'x') t == empty
-- True
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
-- >>> filter (\x -> getPayload x == 'x') "x y x z" :: PictoralTimeline
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

-- | Merge two timelines. Choose second in case of conflicts.
-- For general implementation see /mergeWith/.
merge :: Ord t => Timeline t p -> Timeline t p -> Timeline t p
merge = Data.Timeline.Naive.mergeWith (\_ b -> b)

-- | \( O(n+m) \). Returns timeline union of two timelines. For example,
--
-- >>> Data.Timeline.Naive.mergeWith (\a b -> b) "xxx" "" :: PictoralTimeline
-- xxx
--
-- >>> Data.Timeline.Naive.mergeWith (\a b -> b) "xxx" "   yyy" :: PictoralTimeline
-- xxxyyy
--
-- >>> Data.Timeline.Naive.mergeWith (\a b -> b) "xxx" "yyy" :: PictoralTimeline
-- yyy
--
-- >>> Data.Timeline.Naive.mergeWith (\a b -> b) "xxx" " yyy" :: PictoralTimeline
-- xyyy
--
-- >>> Data.Timeline.Naive.mergeWith (\a b -> b) " xxx" "yyy" :: PictoralTimeline
-- yyyx
--
-- >>> Data.Timeline.Naive.mergeWith (\a b -> b) "xx" "   yy" :: PictoralTimeline
-- xx yy
--
-- >>> Data.Timeline.Naive.mergeWith (\a b -> b) " x y z" "x y z" :: PictoralTimeline
-- xxyyzz
--
-- >>> Data.Timeline.Naive.mergeWith (\a b -> b) "xxxxx" " a b" :: PictoralTimeline
-- xaxbx
--
-- >>> let t1 = "xxx yyy zzz"   :: PictoralTimeline
-- >>> let t2 = "  aaa bbb ccc" :: PictoralTimeline
-- >>> Data.Timeline.Naive.mergeWith (\a b -> b) t1 t2
-- xxaaaybbbzccc
--
-- >>> let t1 = "xxxxxxxx"  :: PictoralTimeline
-- >>> let t2 = " aa bb cc" :: PictoralTimeline
-- >>> Data.Timeline.Naive.mergeWith (\a b -> b) t1 t2
-- xaaxbbxcc
--
-- >>> let t1 = " aa bb cc" :: PictoralTimeline
-- >>> let t2 = "xxxxxxxx" :: PictoralTimeline
-- >>> Data.Timeline.Naive.mergeWith (\a b -> b) t1 t2
-- xxxxxxxxc
mergeWith
  :: Ord t
  => (p -> p -> p) -- ^ resolve function
  -> Timeline t p  -- ^ A
  -> Timeline t p  -- ^ B
  -> Timeline t p
mergeWith f x y = _mergeWith f x y empty

-- @'mergeWith' helper function.
-- Uses reversed accumulator timeline to allow \( O(1) \) access to the last accumulated element.
-- This way it is possible to achieve \( O(n+m) \) total @'mergeWith' complexity.
_mergeWith
  :: Ord t
  => (p -> p -> p)
  -> Timeline t p  -- ^ timeline 1
  -> Timeline t p  -- ^ timeline 2
  -> Timeline t p  -- ^ reversed accumulated timeline
  -> Timeline t p  -- ^ result
_mergeWith _ (Timeline []) (Timeline []) (Timeline []) = empty
_mergeWith _ (Timeline []) (Timeline []) acc           = reverse acc

_mergeWith _ x (Timeline []) (Timeline []) = x
_mergeWith _ (Timeline []) y (Timeline []) = y

_mergeWith f (Timeline (x:xs)) (Timeline []) acc
  = _mergeWith f (Timeline xs) empty (_reversedInsert (flip f) x acc)

_mergeWith f (Timeline []) (Timeline (y:ys)) acc
  = _mergeWith f empty (Timeline ys) (_reversedInsert f y acc)

_mergeWith f t1@(Timeline (x@(Event xi _) : xs)) t2@(Timeline (y@(Event yi _) : ys)) (Timeline [])
  | xi <= yi  = _mergeWith f (Timeline xs) t2            (singleton x)
  | otherwise = _mergeWith f t1            (Timeline ys) (singleton y)

_mergeWith f t1@(Timeline (x@(Event xi _) : xs)) t2@(Timeline (y@(Event yi _) : ys)) acc
  | xi < yi = _mergeWith f (Timeline xs) t2 (_reversedInsert (flip f) x acc)
  | otherwise = _mergeWith f t1 (Timeline ys) (_reversedInsert f y acc)

_reversedInsert
  :: Ord t
  => (p -> p -> p)
  -> Event t p
  -> Timeline t p
  -> Timeline t p
_reversedInsert _    el (Timeline [])     = singleton el
_reversedInsert func el (Timeline (z:zs)) = Timeline $ Prelude.reverse (Event.mergeWith func z el) ++ zs


-- TODO: optimization
-- As we know, Timeline is ascending and has no overlaps (see isValid).
-- So, after each iteration processed interval can be dropped,
-- thus decreasing number of operations to be performed in the next iteration.

-- | Find intersection of the first timeline with the second.
--
-- >>> toString $ (mkPictoralTimeline "xxx") `intersect` (mkPictoralTimeline " yyy")
-- " xx"
--
-- >>> toString $ (mkPictoralTimeline "xxx") `intersect` (mkPictoralTimeline "    yyy")
-- ""
--
-- >>> toString $ (mkPictoralTimeline "xxx yyy") `intersect` (mkPictoralTimeline "  zzz")
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
      = case findIntersectionFlip (map Event.getInterval t) i of
        Just x  -> Just $ Event x p
        Nothing -> Nothing
    findIntersectionFlip x y = findIntersection y x

intersectWith
  :: Ord t
  => (Event t p -> Event t p -> Event t p) -- ^ resolver
  -> Timeline t p -- ^ A
  -> Timeline t p -- ^ B
  -> Timeline t p
intersectWith _ (Timeline []) _ = empty
intersectWith _ _ (Timeline []) = empty
intersectWith
  f
  t1@(Timeline (e1@(Event (Interval (l1, r1)) p1):xs))
  t2@(Timeline (e2@(Event (Interval (l2, r2)) p2):ys))

  -- 5
  | l1 >= r2 = intersectWith f t1 (Timeline ys)

  -- 6
  | r1 <= l2 = intersectWith f (Timeline xs) t2

  -- 1
  | l1 > l2 && r1 < r2
    = Timeline (
      f e1 (Event (mkInterval l1 r1) p2)
      :
      getTimeline (intersectWith f (Timeline xs) (Timeline (Event (mkInterval r1 r2) p2 : ys)))
    )

  -- 2
  | l1 < l2 && r1 > r2
    = Timeline (
      f (Event (mkInterval l2 r2) p1) e2
      :
      getTimeline (intersectWith f (Timeline (Event (mkInterval r2 r1) p1 : xs)) (Timeline ys))
    )

  -- 3
  | l1 == l2 && r1 > r2
    = Timeline (
      f (Event (mkInterval l1 r2) p1) e2
      :
      getTimeline (intersectWith f (Timeline (Event (mkInterval r2 r1) p1 : xs)) (Timeline ys))
    )

  -- 4
  | l1 < l2 && r1 == r2
    = Timeline (
      f (Event (mkInterval l2 r2) p1) e2
      :
      getTimeline (intersectWith f (Timeline xs) (Timeline ys))
    )

  -- 7
  | l1 == l2 && r1 == r2
    = Timeline (f e1 e2 : getTimeline (intersectWith f (Timeline xs) (Timeline ys)))

  -- 9
  | l1 < l2 && r1 < r2
    = Timeline (
      f (Event (mkInterval l2 r1) p1) (Event (mkInterval l2 r1) p2)
      :
      getTimeline (intersectWith f (Timeline xs) (Timeline (Event (mkInterval r1 r2) p2 : ys)))
    )

  -- 8
  | l1 > l2 && r1 > r2
    = Timeline (
      f (Event (mkInterval l1 r2) p1) (Event (mkInterval l1 r2) p2)
      :
      getTimeline (intersectWith f (Timeline (Event (mkInterval r2 r1) p1 : xs)) (Timeline ys))
    )

  -- 10
  | l1 == l2 && r1 < r2
    = Timeline (
      f e1 (Event (mkInterval l1 r1) p2)
      :
      getTimeline (intersectWith f (Timeline xs) (Timeline (Event (mkInterval r1 r2) p2 : ys)))
    )

  -- 11
  | l1 > l2 && r1 == r2
    = Timeline (
      f e1 (Event (mkInterval l1 r1) p2)
      :
      getTimeline (intersectWith f (Timeline xs) (Timeline ys))
    )
  | otherwise = error "One or multiple timelines do not satisfy timeline properties."


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
-- * Transformations

-- | Reverse the timeline.
reverse :: Timeline t p -> Timeline t p
reverse (Timeline xs) = Timeline (Prelude.reverse xs)


-- | Shift all events in time by `n`
--
-- >>> shiftWith (+) 1 "x y z" :: PictoralTimeline
--  x y z
--
-- >>> shiftWith (+) 2 "x y z" :: PictoralTimeline
--   x y z
--
-- >>> shiftWith (+) (-1) "  x y z" :: PictoralTimeline
--  x y z
shiftWith
  :: Ord t
  => (t -> t -> t)
  -> t
  -> Timeline t p
  -> Timeline t p
shiftWith f n (Timeline xs) = unsafeFromList $ map (shiftWith' f) xs
  where
    shiftWith' func (Event i p) = Event (Interval.shiftWith func n i) p

-- | Monadic bind.
--
-- (>>=) :: Timeline t a -> (a -> Timeline t b) -> Timeline t b
--
-- timeline >>= f = flatMapWith const timeline (\event -> f $ payload event)
--
-- >>> f a b = b
-- >>> g (Event i p) = singleton (Event i 'a')
-- >>> t = "xxx yyy" :: PictoralTimeline
-- >>> flatMapWith f t g
-- aaa aaa
flatMapWith
  :: Ord t
  => (b -> b -> b)
  -> Timeline t a
  -> (Event t a -> Timeline t b)
  -> Timeline t b
flatMapWith _ (Timeline []) _ = empty
flatMapWith f (Timeline xs) g = fromListWith f $ join $ map getTimeline (fmap g xs)

-----------------------------------------------------------------------------
-- * Conversion

-- | Convert Timeline to list of Intervals
toList :: Timeline t p -> [Event t p]
toList = getTimeline

isAscending :: Ord a => [a] -> Bool
isAscending xs = and (zipWith (<) xs (Prelude.drop 1 xs))

isValid :: Ord t => Timeline t p -> Bool
isValid = isAscending . map Event.getInterval . toList

-----------------------------------------------------------------------------
-- * Helpers

-- | General map function.
-- Basically, an interface for monotonic functions.
_map
  :: (Event t p -> Event t p)
  -> Timeline t p
  -> Timeline t p
_map f (Timeline xs) = Timeline (map f xs)

findIntersection
  :: Ord t
  => Interval t         -- ^ interval to find intersection with.
  -> [Interval t]       -- ^ intervals in which to search.
  -> Maybe (Interval t) -- ^ intersection or Nothing.
findIntersection i xs = asum (map (Interval.intersect i) xs)

-- | Update events using a reference timeline schedule.
-- All of the events from the second timeline are overlayed
-- over events from the first timeline (reference).
-- Gaps from both the original timeline and the reference timeline are preserved.
--
-- See 'withReference_' for a specialized version.
withReference
  :: (Num rel, Ord rel)
  => (abs -> abs -> rel) -- ^ time difference
  -> (abs -> rel -> abs) -- ^ time addition
  -> (a -> b -> c)       -- ^ payload combinator
  -> Timeline abs a      -- ^ reference timeline 
  -> Timeline rel b      -- ^ target timeline
  -> Timeline abs c      -- ^ target timeline overlaid over reference 
withReference diff add f = unsafeIntersectionWithEvent combine . shrink diff
  where
    combine i x y = Event (Interval (from, to)) (f a b)
      where
        from = add f3 (f1 - f2)
        to = add f3 (t1 - f2)
        Interval (f1, t1) = i
        Event (Interval (f2, _)) (Event (Interval (f3, _)) a) = x
        Event _ b = y

-- | Update events using a reference timeline schedule.
--
-- All of the events from the second timeline are overlayed
-- over events from the first timeline (reference):
--
-- >>> t1 = "    xxxx  yyyy    zzzz " :: PictoralTimeline
-- >>> t2 = "123456789ABCDEF" :: PictoralTimeline
-- >>> t1
--     xxxx  yyyy    zzzz
-- >>> withReference_ t1 t2
--     1234  5678    9ABC
--
-- Gaps from both the original timeline and the reference timeline are preserved:
--
-- >>> t1 = "    xxxx  yyyy    zzzz " :: PictoralTimeline
-- >>> t2 = "12 45  89A CDEF" :: PictoralTimeline
-- >>> t1
--     xxxx  yyyy    zzzz
-- >>> withReference_ t1 t2
--     12 4  5  8    9A C
withReference_
  :: (Num t, Ord t)
  => Timeline t a
  -> Timeline t b
  -> Timeline t b
withReference_ = withReference (-) (+) (flip const)

-- | /O(N)./ Shrink an (absolute) timeline by removing all the gaps between events.
-- The result is a (relative) timeline with original (absolute) events.
--
-- >>> getPayload <$> shrink (-) "  xxx yyyy   zzz" :: PictoralTimeline
-- xxxyyyyzzz
shrink
  :: (Num rel)
  => (abs -> abs -> rel)
  -> Timeline abs a
  -> Timeline rel (Event abs a)
shrink diff = unsafeFromList . shrink' . toList
  where
    shrink' events = zipWith Event intervals events
      where
        intervals = scanl1 step (map (toRel . Event.getInterval) events)
        step (Interval (_, prevTo)) (Interval (_, dur)) = Interval (prevTo, prevTo + dur)
        toRel (Interval (from, to)) = Interval (0, to `diff` from)

-- | /O(N)./ Intersection of two timelines with a custom combining function
-- that takes into account intersection interval and both source events
-- and can construct a new event with potentially different type of time.
--
-- This function is "unsafe" because it assumes that combining function
-- maps its first argument (interval of the intersection) monotonically.
--
-- This allows intersection to be fast (linear) and lazy.
--
-- >>> t1 = "xxxx yyyy" :: PictoralTimeline
-- >>> t2 = "1 111 11"  :: PictoralTimeline
-- >>> t1
-- xxxx yyyy
-- >>> t2
-- 1 111 11
-- >>> unsafeIntersectionWithEvent (\i e _ -> e { Event.getInterval = i }) t1 t2
-- x xx  yy
unsafeIntersectionWithEvent
  :: (Ord t)
  => (Interval t -> Event t a -> Event t b -> Event t' c)
  -> Timeline t a
  -> Timeline t b
  -> Timeline t' c
unsafeIntersectionWithEvent f as bs = unsafeFromList (go (toList as) (toList bs))
  where
    go [] _ = []
    go _ [] = []
    go (x:xs) (y:ys)
      | r1 < l2   = go xs (y:ys)
      | r2 < l1   = go (x:xs) ys
      | r1 <= r2  = z : go xs (y:ys)
      | otherwise = z : go (x:xs) ys
      where
        Event (Interval (l1, r1)) _ = x
        Event (Interval (l2, r2)) _ = y
        i = mkInterval (max l1 l2) (min r1 r2)
        z = f i x y

duplicateEvents :: Timeline t a -> Timeline t (Event t a)
duplicateEvents = unsafeFromList . map dup . toList
  where
    dup (Event i a) = Event i (Event i a)
