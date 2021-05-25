{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Timeline.Lazy where

import qualified Data.Timeline.Strict as Strict
import qualified Data.Timeline.Naive  as Naive
import           Data.Timeline.Pictoral as Pic
import           Prelude                        hiding (head, tail)
import           Data.Timeline.Event 
import           Data.Timeline.Interval        
import           Data.List (sortOn)
import           Data.String            (IsString (..))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Prelude hiding (take, takeWhile, subtract, null, filter, drop, dropWhile)
-- >>> import Data.Timeline.Pictoral
-- >>> import Data.Timeline.Event
-- $setup

data Timeline t p
  = Empty
  | Chunk !(Strict.Timeline t p) (Timeline t p)
  deriving (Functor, Foldable, Traversable, Eq)

instance (Ord t, Num t) => IsString (Timeline t Char) where
  fromString = fromNaive . Pic.mkPictoralTimeline

instance Integral t => Show (Timeline t Char) where
  show = show . toNaive
  
-- $invariant
strictInvariant :: Timeline t p -> Bool
strictInvariant Empty = True
strictInvariant (Chunk c t)
  | Strict.size c > 0 = strictInvariant t
  | otherwise = error "Data.Timeline.Naive: invariant violation"
  
lazyInvariant :: Timeline t p -> Timeline t p
lazyInvariant Empty = Empty
lazyInvariant (Chunk c t)
  | Strict.size c > 0 = Chunk c (lazyInvariant t)
  | otherwise = error "Data.Timeline.Naive: invariant violation"       
  
-- | Smart constructor for chunk. Preserves invariants.
chunk :: Strict.Timeline t p -> Timeline t p -> Timeline t p 
chunk x t
  | Strict.size x == 0 = t
  | otherwise = Chunk x t

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

fromListWith :: Ord t => (p -> p -> p) -> [Event t p] -> Timeline t p
fromListWith f = accumulate empty . sortOn getInterval
  where
    accumulate Empty []     = Empty
    accumulate Empty (e:es) = accumulate (singleton e) es
    accumulate t []         = t
    accumulate (Chunk c t) (e:es)
      | Strict.size c < chunkSize = accumulate (Chunk (Strict.insertWith f e c) Empty) es
      | otherwise                 = Chunk c (accumulate t (e:es)) 
    
-- | /O(N)./ Create timeline from list without preserving non-overlapping invariant.
-- Useful if event list already has no conflicts and is sorted.   
unsafeFromList :: Ord t => [Event t p] -> Timeline t p
unsafeFromList = accumulate empty . sortOn getInterval
  where
    accumulate :: Timeline t p -> [Event t p] -> Timeline t p
    accumulate Empty []     = Empty
    accumulate Empty (e:es) = accumulate (singleton e) es
    accumulate t []         = t 
    accumulate (Chunk c t) (e:es)
      | Strict.size c < chunkSize = accumulate (Chunk (Strict.unsafeSnoc c e) Empty) es
      | otherwise                 = Chunk c (accumulate t (e:es))

-- | /O(N)./ Create Lazy timeline from Naive.
fromNaive :: Ord t => Naive.Timeline t p -> Timeline t p
fromNaive (Naive.Timeline events) = unsafeFromList events 

-- | /O(1)./ Create Lazy timeline from Strict.
fromStrict :: Strict.Timeline t p -> Timeline t p
fromStrict t = Chunk t Empty 

fromStricts :: [Strict.Timeline t p] -> Timeline t p
fromStricts = foldr chunk Empty

-----------------------------------------------------------------------------
-- * Conversion

toList :: Ord t => Timeline t p -> [Event t p]
toList Empty      = []
toList (Chunk x xs) = Strict.toList x <> toList xs

toNaive :: Ord t => Timeline t p -> Naive.Timeline t p
toNaive = Naive.Timeline . toList
      
-----------------------------------------------------------------------------
-- * Combination

unsafeConcat :: Ord t => Timeline t p -> Timeline t p -> Timeline t p
unsafeConcat Empty Empty = Empty
unsafeConcat Empty b     = b
unsafeConcat a Empty     = a
unsafeConcat (Chunk a Empty) b = Chunk a b
unsafeConcat (Chunk a as) b = Chunk a (unsafeConcat as b)

insertWith :: Ord t => (p -> p -> p) -> Event t p -> Timeline t p -> Timeline t p
insertWith f event timeline = error "not implemented"

merge :: Ord t => Timeline t p -> Timeline t p -> Timeline t p
merge = mergeWith (\_ b -> b)

-- | Merge timelines using conflict resolver.
--
-- >>> let a = fromNaive ("aa aa aa aa" :: PictoralTimeline)
-- >>> let b = fromNaive (" bbb   bbb" :: PictoralTimeline)
-- >>> mergeWith (\_ b -> b) a b
-- abbba abbba
--
-- >>> let a = fromNaive ("aa aa aa aa" :: PictoralTimeline)
-- >>> let b = fromNaive ("bbbbbb" :: PictoralTimeline)
-- >>> mergeWith (\_ b -> b) a b
-- bbbbbbaa aa
--
-- >>> let a = fromNaive (" a a a a" :: PictoralTimeline)
-- >>> let b = fromNaive ("bbb b b" :: PictoralTimeline)
-- >>> mergeWith (\_ b -> b) a b
-- bbbababa
mergeWith :: Ord t => (p -> p -> p) -> Timeline t p -> Timeline t p -> Timeline t p
mergeWith _ Empty Empty = Empty
mergeWith _ t     Empty = t
mergeWith _ Empty t     = t

mergeWith f (Chunk a as) (Chunk b Empty) = case mergeChunks f a b of
  Empty -> Empty
  Chunk x Empty -> chunk x as
  Chunk x remaining -> chunk x (mergeWith f as remaining)

mergeWith f (Chunk a Empty) (Chunk b bs) = case mergeChunks f a b of
  Empty -> Empty
  Chunk x Empty -> chunk x bs
  Chunk x remaining -> chunk x (mergeWith f remaining bs)

mergeWith f (Chunk a as) (Chunk b bs) = case mergeChunks f a b of
  Empty             -> Empty
  Chunk x Empty     -> chunk x (mergeWith f as bs)
  Chunk x remaining ->
    if unsafeEndTime remaining < unsafeStartTime as
      then chunk x (mergeWith f (remaining `unsafeConcat` as) bs)
      else chunk x (mergeWith f as (remaining `unsafeConcat` bs))

intersect :: Ord t => Timeline t p -> Timeline t p -> Timeline t p
intersect = intersectWith (\_ b -> b)

intersectWith :: Ord t => (p -> p -> p) -> Timeline t p -> Timeline t p -> Timeline t p
intersectWith = error "not implemented"

difference :: Ord t => Timeline t p -> Timeline t p -> Timeline t p
difference = error "not implemented"

-----------------------------------------------------------------------------
-- * Internals    

chunkSize :: Int
chunkSize = 2

tuplify2 :: Maybe [a] -> Maybe (a, a)
tuplify2 (Just [x, y]) = Just (x, y)
tuplify2 _             = Nothing 

compareChunks :: Ord t => Strict.Timeline t p -> Strict.Timeline t p -> Ordering
compareChunks q w = Strict.startTime q `compare` Strict.startTime w

takeChunksWhileOverlapping 
  :: Ord t
  => Timeline t p
  -> Timeline t p
  -> ((Timeline t p, Timeline t p), (Timeline t p, Timeline t p))
takeChunksWhileOverlapping (Chunk a as) (Chunk b bs)
  | a `Strict.overlaps` b = go a b as bs
  where 
    go x y xs ys
      | compareChunks x y == LT = 
        case xs of
          Chunk z zs | z `Strict.overlaps` y ->
            let ((xs', ys'), suffixes) = go z y zs ys
              in ((Chunk x xs', ys'), suffixes)
          _ -> ((Chunk x Empty, Chunk y Empty), (xs, ys))  
      | otherwise =
        case ys of
          Chunk z zs | z `Strict.overlaps` x ->
            let ((xs', ys'), suffixes) = go x z xs zs
              in ((xs', Chunk y ys'), suffixes)
          _ -> ((Chunk x Empty, Chunk y Empty), (xs, ys))
takeChunksWhileOverlapping as bs = ((Empty, Empty), (as, bs))
          
toSegments 
  :: Ord t 
  => Timeline t p 
  -> Timeline t p 
  -> [Either (Timeline t p) (Timeline t p, Timeline t p)]
toSegments a b = case takeChunksWhileOverlapping a b of
  ((Empty, Empty), (Empty, Empty)) -> []
  ((Empty, Empty), _) -> case (a, b) of
    (Chunk x xs, Chunk y ys)
      | compareChunks x y == LT -> Left (Chunk x Empty) : toSegments xs b
      | otherwise -> Left (Chunk y Empty) : toSegments a ys
    _ -> [Left (unsafeConcat a b)]
  ((px, py), (sx, sy)) -> Right (px, py) : toSegments sx sy 

catEithers 
  :: Ord t
  => [Either (Timeline t p) (Timeline t p, Timeline t p)] 
  -> [Either (Timeline t p) (Timeline t p, Timeline t p)]
catEithers [] = []
catEithers (Left x : Left y : zs) = catEithers (Left (unsafeConcat x y) : zs)
catEithers (Right (x1, y1) : Right (x2, y2) : zs) 
  = catEithers (Right (unsafeConcat x1 x2, unsafeConcat y1 y2) : zs)
catEithers (x : xs) = x : catEithers xs

mergeChunks
  :: Ord t
  => (p -> p -> p)
  -> Strict.Timeline t p
  -> Strict.Timeline t p
  -> Timeline t p
mergeChunks f a b
  | Strict.size a == 0 && Strict.size b == 0 = Empty
  | Strict.size a == 0 = chunk b Empty
  | Strict.size b == 0 = chunk a Empty
  | otherwise = fromStricts $ Strict.toChunksOfSize chunkSize (Strict.mergeW f a b)
