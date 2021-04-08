{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Timeline.Strict where

import           Prelude                hiding (drop, dropWhile, filter)
import           Data.String            (IsString (..))
import           Data.Timeline.Event
import           Data.Timeline.Interval
import qualified Data.Timeline.Naive    as Naive
import qualified Data.Timeline.Pictoral ()
import           Data.Tuple.Extra
import qualified Data.Vector            as V

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Prelude hiding (take, takeWhile, subtract, null, filter, drop, dropWhile)
-- >>> import Data.Timeline.Pictoral
-- $setup

data Timeline t p = Timeline
  { timelinePayload :: !(V.Vector p)
  , timelineFrom    :: !(V.Vector t)
  , timelineTo      :: !(V.Vector t) -- TODO investigate the unboxed vectors + type-level functions
  } deriving (Functor, Foldable, Traversable)

instance (Ord t, Num t) => IsString (Timeline t Char) where
  fromString = fromNaive . fromString

instance Integral t => Show (Timeline t Char) where
  show = show . toNaive

-----------------------------------------------------------------------------
-- * Accessors

isEmpty :: Timeline t p -> Bool
isEmpty (Timeline ps froms tos) = V.null ps || V.null froms || V.null tos

firstEvent :: Timeline t p -> Maybe (Event t p)
firstEvent t@(Timeline ps fs ts)
  | isEmpty t = Nothing
  | otherwise = Just (Event (Interval (V.head fs, V.head ts)) (V.head ps))

-----------------------------------------------------------------------------
-- * Construction

empty :: Timeline t p
empty = Timeline V.empty V.empty V.empty

singleton :: Event t p -> Timeline t p
singleton (Event (Interval (f, t)) p) = Timeline (V.singleton p) (V.singleton f) (V.singleton t)

-- | /O(n)/ Create strict Timeline from three lists.
--
-- >>> payloads = ['a', 'b']
-- >>> froms = [0, 3]
-- >>> tos = [2, 5]
-- >>> fromLists payloads froms tos
-- aa bb
fromLists :: [p] -> [t] -> [t] -> Timeline t p
fromLists p from to
  = fromVectors (V.fromList p) (V.fromList from) (V.fromList to)

-- | /O(1)/ Construct Timeline from three vectors (payloads, from points, to points).
fromVectors :: V.Vector p -> V.Vector t -> V.Vector t -> Timeline t p
fromVectors = Timeline

-- | /O(n)/ Convert Naive structure to Strict.
fromNaive :: Naive.Timeline t p -> Timeline t p
fromNaive = uncurry3 fromLists . unzip3 . map f . Naive.getTimeline
  where
    f (Event (Interval (from, to)) p) = (p, from, to)

unsafeMapTimestampMonotonic :: (t -> t') -> Timeline t p -> Timeline t' p
unsafeMapTimestampMonotonic f tl = tl
  { timelineFrom = f <$> timelineFrom tl
  , timelineTo   = f <$> timelineTo tl
  }

-- | /O(n)/ Insert event into the timeline.
-- 
-- >>> f = (\a b -> b)
-- >>> payloads = ['a']
-- >>> froms = [2]
-- >>> tos   = [5]
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
insertWith f event timeline = fromNaive $ Naive.insert f event (toNaive timeline)

-----------------------------------------------------------------------------
-- * Query

-- | /O(n)/ Filter timeline by payloads.
--
-- >>> let t = "a b cc a" :: PictoralTimeline
-- >>> filter (== 'a') (fromNaive t)
-- a      a
filter :: (p -> Bool) -> Timeline t p -> Timeline t p
filter f = filterEvents g
  where
    g event = f $ getPayload event

-- | /O(n) Filter timeline by events.
--
--
filterEvents
  :: (Event t p -> Bool)
  -> Timeline t p
  -> Timeline t p
filterEvents f (Timeline ps fs ts) = fromVectorsTuple $ V.unzip3 $ V.filter g (V.zip3 ps fs ts)
  where
    g = f . uncurry3 fromTriple
    fromVectorsTuple (a, b, c) = Timeline a b c

-----------------------------------------------------------------------------
-- * Updates

-- | /O(1)/ Delete first /n/ events from the timeline.
drop
  :: Int
  -> Timeline t p
  -> Timeline t p
drop n (Timeline ps fs ts) = Timeline (V.drop n ps) (V.drop n fs) (V.drop n ts)

-- /O(log(n))/ with binary search and V.splitAt? 
delete :: Interval t -> Timeline t p -> Timeline t p
delete (Interval (from, to)) t = error "not implemented"

-----------------------------------------------------------------------------
-- * Combinations

unsafeConcat :: Timeline t p -> Timeline t p -> Timeline t p
unsafeConcat (Timeline ps1 fs1 ts1) (Timeline ps2 fs2 ts2)
  = Timeline (ps1 V.++ ps2) (fs1 V.++ fs2) (ts1 V.++ ts2)
  
-----------------------------------------------------------------------------
-- * Conversion  
-- | /O(n)/ Convert Strict structure to Naive
toNaive
  :: Ord t
  => Timeline t p
  -> Naive.Timeline t p
toNaive (Timeline ps froms tos) = Naive.Timeline $ V.toList $ V.zipWith3 toNaiveEvent ps froms tos
  where
    toNaiveEvent p from to = Event (mkInterval from to) p