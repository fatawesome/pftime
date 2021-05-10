{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Timeline.Lazy where

import qualified Data.Timeline.Strict as Strict
import qualified Data.Timeline.Naive  as Naive
import           Prelude                        hiding (head, tail)
import           Data.Timeline.Event            hiding (mergeWith) 
import           Data.Timeline.Interval         hiding (getInterval)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Prelude hiding (take, takeWhile, subtract, null, filter, drop, dropWhile)
-- >>> import Data.Timeline.Pictoral
-- >>> import Data.Timeline.Event
-- $setup

data Timeline t p
  = Empty
  | Chunk !(Strict.Timeline t p) (Timeline t p)
  deriving (Functor, Foldable, Traversable)
  
instance Integral t => Show (Timeline t Char) where
  show Empty       = ""
  show (Chunk c t) = show c <> ", " <> show t 

-----------------------------------------------------------------------------
-- * Accessors

-- | /O(1)./ Get first event from timeline.
head :: Timeline t p -> Maybe (Event t p)
head Empty       = Nothing
head (Chunk c _) = Strict.head c

unsafeHead :: Timeline t p -> Event t p
unsafeHead Empty       = error "unsafeHead is UNSAFE"
unsafeHead (Chunk c _) = Strict.unsafeHead c

-- | /O(1)./ Get all but first event from timeline.
-- TODO: chunks balancing.
tail :: Timeline t p -> Timeline t p
tail Empty           = Empty
tail (Chunk c Empty) = Chunk (Strict.tail c) Empty
tail (Chunk c t)     = Chunk (Strict.tail c) t

unsafeTail :: Timeline t p -> Event t p
unsafeTail = error "not implemented"

-- | /O(1)./ Get timeline starting time.
startTime :: Timeline t p -> Maybe t
startTime t = case head t of
  Nothing -> Nothing
  Just (Event (Interval (from, _)) _) -> Just from  

unsafeStartTime :: Timeline t p -> t
unsafeStartTime t = from
  where
    Event (Interval (from, _)) _ = unsafeHead t 

-- | /O(N)./ Get timeline ending time.
endTime :: Timeline t p -> Maybe t
endTime Empty = Nothing
endTime t = Just $ unsafeEndTime t

unsafeEndTime :: Timeline t p -> t
unsafeEndTime Empty           = error "unsafeEndTime does not accept empty timeline as input, see `endTime`."
unsafeEndTime (Chunk c Empty) = end (Strict.unsafeLast c)
unsafeEndTime (Chunk _ t)     = unsafeEndTime t

-- | /O(N)./ Get timeline bounds. 
timeBounds :: Timeline t p -> Maybe (t, t)
timeBounds t = tuplify2 $ sequence [startTime t, endTime t]

unsafeTimeBounds :: Timeline t p -> (t, t)
unsafeTimeBounds t = (unsafeStartTime t, unsafeEndTime t)

-----------------------------------------------------------------------------
-- * Construction

empty :: Timeline t p
empty = Empty

singleton :: Event t p -> Timeline t p
singleton e = Chunk (Strict.singleton e) Empty

-- | /O(N)./ Create timeline from list without preserving structure invariants.
-- Useful if event list already has no conflicts and is sorted.   
unsafeFromList :: [Event t p] -> Timeline t p
unsafeFromList = accumulate empty
  where
    accumulate :: Timeline t p -> [Event t p] -> Timeline t p
    accumulate Empty []     = Empty
    accumulate Empty (e:es) = accumulate (singleton e) es
    accumulate t []         = t 
    accumulate (Chunk c t) (e:es)
      | Strict.size c < chunkSize = accumulate (Chunk (Strict.unsafeSnoc c e) Empty) es
      | otherwise                 = Chunk c (accumulate t (e:es))  

-- | /O(N)./ Create Lazy timeline from Naive.
--
-- >>> let t = "xyxyxyxyxyxy" :: PictoralTimeline
-- >>> fromNaive t
fromNaive :: Naive.Timeline t p -> Timeline t p
fromNaive (Naive.Timeline events) = unsafeFromList events 
      
-----------------------------------------------------------------------------
-- * Combination

merge :: Ord t => Timeline t p -> Timeline t p -> Timeline t p
merge = mergeWith (\_ b -> b)

mergeWith :: Ord t => (p -> p -> p) -> Timeline t p -> Timeline t p -> Timeline t p
mergeWith = error "not implemented"

intersect :: Ord t => Timeline t p -> Timeline t p -> Timeline t p
intersect = intersectWith (\_ b -> b)

intersectWith :: Ord t => (p -> p -> p) -> Timeline t p -> Timeline t p -> Timeline t p
intersectWith = error "not implemented"

difference :: Ord t => Timeline t p -> Timeline t p -> Timeline t p
difference = error "not implemented"

-----------------------------------------------------------------------------
-- * Helpers    

chunkSize :: Int
chunkSize = 8

tuplify2 :: Maybe [a] -> Maybe (a, a)
tuplify2 (Just [x, y]) = Just (x, y)
tuplify2 _             = Nothing 
