{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Timeline.Strict where

import           Data.String            (IsString (..))
import           Data.Timeline.Event
import           Data.Timeline.Interval
import qualified Data.Timeline.Naive    as Naive
import qualified Data.Timeline.Pictoral ()
import           Data.Tuple.Extra
import           Data.Vector            (Vector)
import qualified Data.Vector            as Vector

data Timeline t p = Timeline
  { timelinePayload :: !(Vector p)
  , timelineFrom    :: !(Vector t)
  , timelineTo      :: !(Vector t)
  } deriving (Functor, Foldable, Traversable)

instance (Ord t, Num t) => IsString (Timeline t Char) where
  fromString = fromNaive . fromString

instance Integral t => Show (Timeline t Char) where
  show = show . toNaive

-----------------------------------------------------------------------------
-- * Accessors

isEmpty :: Timeline t p -> Bool
isEmpty (Timeline ps froms tos) = Vector.null ps || Vector.null froms || Vector.null tos

-----------------------------------------------------------------------------
-- * Construction

empty :: Timeline t p
empty = Timeline Vector.empty Vector.empty Vector.empty

singleton :: Event t p -> Timeline t p
singleton (Event (Interval (f, t)) p) = Timeline (V.singleton p) (V.singleton f) (V.singleton t) 

-- | Create strict Timeline from three lists.
--
-- >>> payloads = ['a', 'b']
-- >>> froms = [0, 3]
-- >>> tos = [2, 5]
-- >>> fromLists payloads froms tos
-- aa bb
fromLists :: [p] -> [t] -> [t] -> Timeline t p
fromLists p from to
  = fromVectors (Vector.fromList p) (Vector.fromList from) (Vector.fromList to)

fromVectors :: Vector p -> Vector t -> Vector t -> Timeline t p
fromVectors = Timeline

fromNaive :: Naive.Timeline t p -> Timeline t p
fromNaive = uncurry3 fromLists . unzip3 . map f . Naive.getTimeline
  where
    f (Event (Interval (from, to)) p) = (p, from, to)

toNaive
  :: Ord t
  => Timeline t p
  -> Naive.Timeline t p
toNaive (Timeline ps froms tos) = Naive.Timeline $ Vector.toList $ Vector.zipWith3 toNaiveEvent ps froms tos
  where
    toNaiveEvent p from to = Event (mkInterval from to) p

unsafeMapTimestampMonotonic :: (t -> t') -> Timeline t p -> Timeline t' p
unsafeMapTimestampMonotonic f tl = tl
  { timelineFrom = f <$> timelineFrom tl
  , timelineTo   = f <$> timelineTo tl
  }

-- | Insert event into the timeline.
--
-- >>> f = (\_ b = b)
-- >>> payloads = ['a']
-- >>> froms = [2]
-- >>> tos   = [5]
--
-- >>> t = fromLists payloads froms tos
-- >>> t
--   aaa
--
-- case 1
-- >>> event = Event (mkInterval 1 6) 'b'
-- >>> insertWith f event t
--  bbbbb
--
-- case 2
-- >>> event = Event (mkInterval 3 4) 'b'
-- >>> insertWith f event t
--   aba
--
-- case 3
-- >>> event = Event (mkInterval 2 4) 'b'
-- >>> insertWith f event t
--   bba
--
-- case 4
-- >>> event = Event (mkInterval 3 5) 'b'
-- >>> insertWith f event t
--   abb
--
-- case 5
-- >>> event = Event (mkInterval 0 1) 'b'
-- >>> insertWith f event t
-- b aaa
--
-- case 6
-- >>> event = Event (mkInterval 6 7) 'b'
-- >>> insertWith f event t
--   aaa b
--
-- case 7
-- >>> event = Event (mkInterval 2 5) 'b'
-- >>> insertWith f event t
--   bbb
--
-- case 8
-- >>> event = Event (mkInterval 1 3) 'b'
-- >>> insertWith f event t
--  bbaa
--
-- case 9
-- >>> event = Event (mkInterval 4 6) 'b'
-- >>> insertWith f event t
--   aabb
--
-- case 10
-- >>> event = Event (mkInterval 2 6) 'b'
-- >>> insertWith f event t
--   bbbb
--
-- case 11
-- >>> event = Event (mkInterval 1 5) 'b'
-- >>> insertWith f event t
--  bbbb
-- 
-- TODO: rewrite not using lazy timeline.
insertWith
  :: Ord t
  => (p -> p -> p)
  -> Event t p
  -> Timeline t p
  -> Timeline t p
insertWith f event timeline
  | isEmpty timeline = empty
  | otherwise = insertImpl event timeline

insertImpl
  :: Ord t
  => Event t p
  -> Timeline t p
  -> Timeline t p
insertImpl
  e@(Event (Interval (from1, to1)) p1)
  (Timeline ps froms tos)
  -- case 5
  | to1 < from2
    = Timeline (Vector.cons p1 ps) (Vector.cons from1 froms) (Vector.cons to1 tos)

  | otherwise = error "not yet"
  where
    p2 = Vector.head ps
    from2 = Vector.head froms
    to2 = Vector.head tos
    maybeFromIdx = Vector.findIndex (> from1) froms
