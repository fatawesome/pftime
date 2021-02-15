{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Timeline.Strict where

import qualified Data.Vector as Vector
import qualified Data.Timeline.Naive as Naive
import           Data.Timeline.Event
import           Data.Timeline.Interval
import           Data.Tuple.Extra

data Timeline t p = Timeline
  { timelinePayload :: !(Vector.Vector p)
  , timelineFrom     :: !(Vector.Vector t)
  , timelineTo       :: !(Vector.Vector t)
  } deriving (Functor, Foldable, Traversable)
  
-- | Create strict Timeline from three lists.
-- 
-- >>> payloads = ['a', 'b']
-- >>> froms = [0, 3]
-- >>> tos = [2, 5]
-- >>> fromLists payloads froms tos :: PictoralTimeline
-- "aa bb"
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
