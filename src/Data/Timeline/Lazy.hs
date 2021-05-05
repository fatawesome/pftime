module Data.Timeline.Lazy where

import qualified Data.Timeline.Strict as Strict
import           Data.Timeline.Event            hiding (mergeWith) 

data Timeline t p
  = Empty
  | Chunk !(Strict.Timeline t p) (Timeline t p)

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
startTime = error "not implemented"

unsafeStartTime :: Timeline t p -> t
unsafeStartTime = error "not implemented"

-- | /O(N)./ Get timeline ending time.
endTime :: Timeline t p -> Maybe t
endTime = error "not implemented"

unsafeEndTime :: Timeline t p -> t
unsafeEndTime = error "not implemented"

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
singleton = error "not implemented"



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

tuplify2 :: Maybe [a] -> Maybe (a, a)
tuplify2 (Just [x, y]) = Just (x, y)
tuplify2 _             = Nothing 
