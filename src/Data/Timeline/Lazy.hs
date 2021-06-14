{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
--{-# LANGUAGE ScopedTypeVariables #-}

module Data.Timeline.Lazy where

import qualified Data.Timeline.Strict as Strict
import qualified Data.Timeline.Naive  as Naive
import           Data.Timeline.Pictoral as Pic
import           Prelude                        hiding (head, tail)
import           Data.Timeline.Event 
import           Data.Timeline.Interval         hiding (difference)     
import           Data.List   (sortOn)
import           Data.String (IsString (..))
import           Test.QuickCheck                hiding (shrink)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Prelude hiding (take, takeWhile, subtract, null, filter, drop, dropWhile)
-- >>> import Data.Timeline.Pictoral
-- >>> import Data.Timeline.Event
-- $setup

data Timeline t p
  = Empty
  | Chunk !(Strict.Timeline t p) (Timeline t p)
  deriving (Functor, Eq, Generic, NFData)

instance (Ord t, Num t) => IsString (Timeline t Char) where
  fromString = fromNaive . Pic.mkPictoralTimeline

--instance Ord t => Foldable (Timeline t p) where
--  foldr f zero


instance Integral t => Show (Timeline t Char) where
  show = show . toNaive
  
instance (Ord t, Arbitrary t, Arbitrary p) => Arbitrary (Timeline t p) where
  arbitrary = fromListWith (\_ b -> b) <$> arbitraryEventList
  
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
insertWith f event timeline = mergeWith f timeline (singleton event)

merge :: Ord t => Timeline t p -> Timeline t p -> Timeline t p
merge = mergeWith (\_ b -> b)

-- | More efficient implementation of merge function. 
-- Applies conflict resolving only where needed. 
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
mergeWith f a b = foldr1 unsafeConcat segments
  where
    segments = map resolveConflicts (catEithers $ toSegments a b)
    resolveConflicts (Left x) = x
    resolveConflicts (Right (x, y)) = _mergeOverlappingWith f x y
    
_mergeOverlappingWith :: Ord t => (p -> p -> p) -> Timeline t p -> Timeline t p -> Timeline t p
_mergeOverlappingWith _ Empty Empty = Empty
_mergeOverlappingWith _ t     Empty = t
_mergeOverlappingWith _ Empty t     = t

_mergeOverlappingWith f (Chunk a as) (Chunk b Empty) = case mergeChunks f a b of
  Empty -> Empty
  Chunk x Empty -> chunk x as
  Chunk x remaining -> chunk x (_mergeOverlappingWith f as remaining)

_mergeOverlappingWith f (Chunk a Empty) (Chunk b bs) = case mergeChunks f a b of
  Empty -> Empty
  Chunk x Empty -> chunk x bs
  Chunk x remaining -> chunk x (_mergeOverlappingWith f remaining bs)

_mergeOverlappingWith f (Chunk a as) (Chunk b bs) = case mergeChunks f a b of
  Empty             -> Empty
  Chunk x Empty     -> chunk x (mergeWith f as bs)
  Chunk x remaining ->
    if unsafeEndTime remaining < unsafeStartTime as
      then chunk x (_mergeOverlappingWith f (remaining `unsafeConcat` as) bs)
      else chunk x (_mergeOverlappingWith f as (remaining `unsafeConcat` bs))

    
intersect :: Ord t => Timeline t p -> Timeline t p -> Timeline t p
intersect = intersectWith (\_ b -> b)

-- | /O(N+M)./ Intersect two timelines using conflict resolving function.
-- 
-- >>> let x = fromNaive ("xxxxx" :: PictoralTimeline)
-- >>> let y = fromNaive (" y y " :: PictoralTimeline)
-- >>> intersectWith (\a b -> a) x y
--  x x
-- 
-- >>> let x = fromNaive (" x x " :: PictoralTimeline)
-- >>> let y = fromNaive ("yyyyy" :: PictoralTimeline)
-- >>> intersectWith (\a b -> a) x y
--  x x
-- 
-- >>> let x = fromNaive ("xxx " :: PictoralTimeline)
-- >>> let y = fromNaive (" yyy" :: PictoralTimeline)
-- >>> intersectWith (\a b -> a) x y
--  xx
--
-- >>> let x = fromNaive ("xxx xxx xxx" :: PictoralTimeline)
-- >>> let y = fromNaive ("  yyy yyy  " :: PictoralTimeline)
-- >>> intersectWith (\a b -> a) x y
--   x x x x  
--
-- >>> let x = fromNaive ("  xxx" :: PictoralTimeline)
-- >>> let y = fromNaive ("yyy  " :: PictoralTimeline)
-- >>> intersectWith (\a b -> a) x y
--   x
--
-- >>> let x = fromNaive ("xx xx xx xx xx xx"  :: PictoralTimeline)
-- >>> let y = fromNaive (" yy yy yy yy yy yy" :: PictoralTimeline)
-- >>> intersectWith (\a b -> a) x y
--  x  x  x  x  x  x
--
-- >>> let x = fromNaive (" xx xx xx xx xx xx"  :: PictoralTimeline)
-- >>> let y = fromNaive ("yy yy yy yy yy yy" :: PictoralTimeline)
-- >>> intersectWith (\a b -> a) x y
--  x  x  x  x  x  x
intersectWith :: Ord t => (p -> p -> p) -> Timeline t p -> Timeline t p -> Timeline t p
intersectWith _ Empty _ = Empty
intersectWith _ _ Empty = Empty
intersectWith f t1@(Chunk x xs) t2@(Chunk y ys) 
  = if left <= right
    then Chunk (Strict.intersectWith' f x y) next
    else next
  where
    left  = max (Strict.unsafeStartTime x) (Strict.unsafeStartTime y)
    right = min (Strict.unsafeEndTime x) (Strict.unsafeEndTime y) 
    next = if Strict.unsafeEndTime x < Strict.unsafeEndTime y
      then intersectWith f xs t2
      else intersectWith f t1 ys 

intersectWithIntervalFunc 
  :: Ord t 
  => (Interval t -> Event t a -> Event t b -> Event t' c)
  -> Timeline t a 
  -> Timeline t b 
  -> Timeline t' c
intersectWithIntervalFunc _ Empty _ = Empty
intersectWithIntervalFunc _ _ Empty = Empty
intersectWithIntervalFunc f t1@(Chunk x xs) t2@(Chunk y ys) 
  = if left <= right
    then Chunk (Strict.Timeline $ Strict.unsafeIntersectionWithEvent' f (Strict.getTimeline x) (Strict.getTimeline y)) next
    else next
  where
    left  = max (Strict.unsafeStartTime x) (Strict.unsafeStartTime y)
    right = min (Strict.unsafeEndTime x) (Strict.unsafeEndTime y) 
    next = if Strict.unsafeEndTime x < Strict.unsafeEndTime y
      then intersectWithIntervalFunc f xs t2
      else intersectWithIntervalFunc f t1 ys 

-- | /O(N+M)./ Find how first timeline is different from second.
--
-- >>> let t1 = fromNaive ("xxx" :: PictoralTimeline)
-- >>> let t2 = fromNaive ("yyy" :: PictoralTimeline)
-- >>> difference t1 t2 == empty
-- True
--
-- >>> let t1 = fromNaive ("xxx    " :: PictoralTimeline)
-- >>> let t2 = fromNaive ("    yyy" :: PictoralTimeline)
-- >>> difference t1 t2
-- xxx
--
-- >>> let t1 = fromNaive ("xxx  " :: PictoralTimeline)
-- >>> let t2 = fromNaive ("  yyy" :: PictoralTimeline)
-- >>> difference t1 t2
-- xx
--
-- >>> let t1 = fromNaive ("xxx yyy" :: PictoralTimeline)
-- >>> let t2 = fromNaive ("xx   yy" :: PictoralTimeline)
-- >>> difference t1 t2
--   x y
--
-- >>> let t1 = fromNaive (" yyy" :: PictoralTimeline)
-- >>> let t2 = fromNaive ("yxxx" :: PictoralTimeline)
-- >>> difference t1 t2 == empty
-- True
--
-- >>> let t1 = fromNaive ("yxxxx" :: PictoralTimeline)
-- >>> let t2 = fromNaive ("yxxx" :: PictoralTimeline)
-- >>> difference t1 t2
--     x
--
-- >>> let t1 = fromNaive ("xxxx" :: PictoralTimeline)
-- >>> let t2 = fromNaive ("yyxx" :: PictoralTimeline)
-- >>> difference t1 t2 == empty
-- True
--
-- >>> let t1 = fromNaive ("x x x x " :: PictoralTimeline)
-- >>> let t2 = fromNaive (" y y y y" :: PictoralTimeline)
-- >>> difference t1 t2
-- x x x x
--
-- >>> let t1 = fromNaive ("xx xx xx xx " :: PictoralTimeline)
-- >>> let t2 = fromNaive (" y  y  y  y" :: PictoralTimeline)
-- >>> difference t1 t2
-- x  x  x  x
difference :: Ord t => Timeline t p -> Timeline t p -> Timeline t p
difference Empty _ = Empty
difference x Empty = x
difference a@(Chunk x xs) b@(Chunk y ys)
  | chunkStart x >= chunkEnd y = difference a ys
  | chunkEnd x <= chunkStart y = Chunk x (difference xs b)
  | otherwise = go $ x `Strict.difference'` y
  where
    go diff
      | Strict.isEmpty diff = if chunkEnd x <= chunkEnd y
        then difference xs b
        else difference a ys
      | otherwise = case chunkEnd diff `compare` chunkEnd y of
                      LT -> Chunk diff (difference xs b)
                      EQ -> Chunk diff (difference xs ys)
                      GT -> difference (Chunk diff xs) ys

-- | Update events using a reference timeline schedule.
--
-- All of the events from the second timeline are overlayed
-- over events from the first timeline (reference):
--
-- >>> t1 = fromNaive ("    xxxx  yyyy    zzzz " :: PictoralTimeline)
-- >>> t2 = fromNaive ("123456789ABCDEF" :: PictoralTimeline)
-- >>> t1
--     xxxx  yyyy    zzzz
-- >>> withReference_ t1 t2
--     1234  5678    9ABC
--
-- Gaps from both the original timeline and the reference timeline are preserved:
--
-- >>> t1 = fromNaive ("    xxxx  yyyy    zzzz " :: PictoralTimeline)
-- >>> t2 = fromNaive ("12 45  89A CDEF" :: PictoralTimeline)
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
  
-- | Update events using a reference timeline schedule.
-- All of the events from the second timeline are overlayed
-- over events from the first timeline (reference).
-- Gaps from both the original timeline and the reference timeline are preserved.                 
withReference
  :: (Num rel, Num abs, Ord rel, Ord abs)
  => (abs -> abs -> rel) -- ^ time difference
  -> (abs -> rel -> abs) -- ^ time addition
  -> (a -> b -> c)       -- ^ payload combinator
  -> Timeline abs a      -- ^ reference timeline 
  -> Timeline rel b      -- ^ target timeline
  -> Timeline abs c      -- ^ target timeline overlaid over reference
withReference diff add f = intersectWithIntervalFunc combine . shrinkChunks diff
  where
    combine i x y = Event (mkInterval from to) (f a b)
      where
        from = add f3 (f1 - f2)
        to = add f3 (t1 - f2)
        Interval (f1, t1) = i
        Event (Interval (f2, _)) (Event (Interval (f3, _)) a) = x
        Event _ b = y

-- | /O(N)./ Shrink an (absolute) timeline by removing all the gaps between events.
-- The result is a (relative) timeline with original (absolute) events.
--
shrinkChunks
--  :: forall abs rel a
  :: (Num rel, Num abs, Ord rel)
  => (abs -> abs -> rel)
  -> Timeline abs a
  -> Timeline rel (Event abs a)
shrinkChunks _ Empty = Empty
shrinkChunks diff timeline = shrink' 0 timeline
  where
    shrink' _ Empty = Empty
    shrink' to (Chunk x xs) = Chunk newChunk (shrink' nextStart xs)
      where
        newChunk = Strict.shrinkTo to diff x
        nextStart = chunkEnd newChunk

-----------------------------------------------------------------------------
-- * Internals    

chunkSize :: Int
chunkSize = 128

tuplify2 :: Maybe [a] -> Maybe (a, a)
tuplify2 (Just [x, y]) = Just (x, y)
tuplify2 _             = Nothing 

compareChunks :: Ord t => Strict.Timeline t p -> Strict.Timeline t p -> Ordering
compareChunks q w = Strict.endTime q `compare` Strict.endTime w

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

-- Интересно побенчмаркать то, когда лучше это делать
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

chunkStart :: Strict.Timeline t p -> t
chunkStart = Strict.unsafeStartTime

chunkEnd :: Strict.Timeline t p -> t
chunkEnd = Strict.unsafeEndTime
