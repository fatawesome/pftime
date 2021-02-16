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
import qualified Data.Vector            as Vector

data Timeline t p = Timeline
  { timelinePayload :: !(Vector.Vector p)
  , timelineFrom    :: !(Vector.Vector t)
  , timelineTo      :: !(Vector.Vector t)
  } deriving (Functor, Foldable, Traversable)

instance (Ord t, Num t) => IsString (Timeline t Char) where
  fromString = fromNaive . fromString

instance Integral t => Show (Timeline t Char) where
  show = show . toNaive

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

fromVectors :: Vector.Vector p -> Vector.Vector t -> Vector.Vector t -> Timeline t p
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
