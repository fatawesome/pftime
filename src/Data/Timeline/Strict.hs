{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Timeline.Strict where

import           Prelude                hiding (drop, dropWhile, filter, head, last, splitAt)
import           Data.String            (IsString (..))
import           Data.Generics.Aliases
import           Data.Timeline.Event
import           Data.Timeline.Interval hiding (overlaps, includes)
import qualified Data.Timeline.Naive    as Naive
import qualified Data.Timeline.Pictoral as Pic (mkPictoralTimeline)
import           Data.Tuple.Extra
import qualified Data.Vector            as V
import           Test.QuickCheck           hiding (shrink)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Prelude hiding (take, takeWhile, subtract, null, filter, drop, dropWhile)
-- >>> import Data.Timeline.Pictoral
-- $setup

data Timeline t p = Timeline {
  getTimeline :: !(V.Vector (Event t p))
} deriving (Functor, Eq)

instance (Ord t, Num t) => IsString (Timeline t Char) where
  fromString = fromNaive . Pic.mkPictoralTimeline

instance Integral t => Show (Timeline t Char) where
  show = show . toNaive
  
instance (Ord t, Arbitrary t, Arbitrary p) => Arbitrary (Timeline t p) where
  arbitrary = fromNaive . Naive.fromListWith (\_ b -> b) <$> arbitraryEventList

-----------------------------------------------------------------------------
-- * Accessors

size :: Timeline t p -> Int
size t = V.length $ getTimeline t

isEmpty :: Timeline t p -> Bool
isEmpty (Timeline xs) = V.null xs

head :: Timeline t p -> Maybe (Event t p)
head (Timeline xs) = V.headM xs
  
tail :: Timeline t p -> Timeline t p
tail (Timeline xs) = Timeline (V.tail xs)

last :: Timeline t p -> Maybe (Event t p)
last (Timeline xs) = V.lastM xs

unsafeHead :: Timeline t p -> Event t p
unsafeHead (Timeline xs) = V.head xs

unsafeLast :: Timeline t p -> Event t p
unsafeLast (Timeline xs) = V.last xs

startTime :: Timeline t p -> Maybe t
startTime timeline = case head timeline of
  Nothing -> Nothing
  Just (Event (Interval (from, _)) _) -> Just from

endTime :: Timeline t p -> Maybe t
endTime timeline = case last timeline of
  Nothing -> Nothing
  Just (Event (Interval (_, to)) _) -> Just to

unsafeStartTime :: Timeline t p -> t
unsafeStartTime timeline = from
  where
    (Event (Interval (from, _)) _) = unsafeHead timeline

unsafeEndTime :: Timeline t p -> t
unsafeEndTime timeline = to
  where
    (Event (Interval (_, to)) _) = unsafeLast timeline

bounds :: Timeline t p -> Maybe (t, t)
bounds x
  | isEmpty x = Nothing
  | otherwise = Just (unsafeStartTime x, unsafeEndTime x)

unsafeBounds :: Timeline t p -> (t, t)
unsafeBounds x = (unsafeStartTime x, unsafeEndTime x)

takeAtIndex :: Int -> Timeline t p -> Maybe (Event t p)
takeAtIndex i (Timeline xs) = xs V.!? i 
  
unsafeTakeAtIndex :: Int -> Timeline t p -> Event t p
unsafeTakeAtIndex i (Timeline xs) = xs V.! i

-----------------------------------------------------------------------------
-- * Predicates

overlaps :: Ord t => Timeline t p -> Timeline t p -> Bool
overlaps x y
  | isEmpty x || isEmpty y = False
  | otherwise = unsafeStartTime x < unsafeEndTime y && unsafeStartTime y < unsafeEndTime x

-----------------------------------------------------------------------------
-- * Construction

empty :: Timeline t p
empty = Timeline V.empty

singleton :: Event t p -> Timeline t p
singleton x = Timeline (V.singleton x)
  
unsafeFromEvents :: V.Vector (Event t p) -> Timeline t p
unsafeFromEvents = error "not implemented"
  where
    event (Event (Interval (l, r)) p) = (p, l, r) 
    
fromList :: [Event t p] -> Timeline t p
fromList = Timeline . V.fromList

-- | /O(n)/ Convert Naive structure to Strict.
fromNaive :: Naive.Timeline t p -> Timeline t p
fromNaive = fromList . Naive.getTimeline

-- | /O(n)/ Insert event into the timeline.
--
-- >>> f = (\a b -> b)
-- >>> x = Event (mkInterval 2 5) 'a'
-- >>> t = singleton x
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
insertWith
  :: Ord t
  => (p -> p -> p)
  -> Event t p
  -> Timeline t p
  -> Timeline t p
insertWith f event timeline = mergeW f timeline (singleton event) 

unsafeSnoc :: Timeline t p -> Event t p -> Timeline t p
unsafeSnoc (Timeline xs) x = Timeline (V.snoc xs x)  

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
filterEvents
  :: (Event t p -> Bool)
  -> Timeline t p
  -> Timeline t p
filterEvents f (Timeline xs) = Timeline (V.filter f xs)

-- | /O(log(n)/ Search for events which happen during given time interval.
-- If there are no such events, returns empty timeline.
search :: Ord t => Interval t -> Timeline t p -> Timeline t p
search interval (Timeline xs) = Timeline (binarySearchEvents interval xs)

--searchIndices :: Ord t => Interval t -> Timeline t p -> (Int, Int)


-- | /O(log(n))/ Binary search in vector with entries of Event type.
--
-- >>> let t = "aaaaa" :: PictoralTimeline
-- >>> binarySearchEvents (mkInterval 1 3) (getTimeline $ fromNaive t) == V.singleton (Event (mkInterval 1 3) 'a')
-- True
--
-- >>> let t = "aaaaa" :: PictoralTimeline
-- >>> binarySearchEvents (mkInterval 0 7) (getTimeline $ fromNaive t) == V.singleton (Event (mkInterval 0 5) 'a')
-- True
--
-- >>> let t = "  aaa" :: PictoralTimeline
-- >>> binarySearchEvents (mkInterval 0 4) (getTimeline $ fromNaive t) == V.singleton (Event (mkInterval 2 4) 'a')
-- True
--
-- >>> let t = "aaa bbb" :: PictoralTimeline
-- >>> binarySearchEvents (mkInterval 0 7) (getTimeline $ fromNaive t) == V.fromList [(Event (mkInterval 0 3) 'a'), (Event (mkInterval 4 7) 'b')]
-- True
--
-- >>> let t = "aaa bbb" :: PictoralTimeline
-- >>> binarySearchEvents (mkInterval 3 4) (getTimeline $ fromNaive t) == V.empty
-- True
--
-- >>> let t = "aaa bbb" :: PictoralTimeline
-- >>> binarySearchEvents (mkInterval 2 5) (getTimeline $ fromNaive t) == V.fromList [(Event (mkInterval 2 3) 'a'), (Event (mkInterval 4 5) 'b')]
-- True
--
-- >>> let t = "aaa bbb ccc" :: PictoralTimeline
-- >>> binarySearchEvents (mkInterval 2 9) (getTimeline $ fromNaive t) == V.fromList [(Event (mkInterval 2 3) 'a'), (Event (mkInterval 4 7) 'b'), (Event (mkInterval 8 9) 'c')]
-- True
binarySearchEvents :: (Ord t) => Interval t -> V.Vector (Event t p) -> V.Vector (Event t p)
binarySearchEvents i@(Interval (from, to)) events
  | V.null events = V.empty
  | f >= to       = binarySearchEvents i as
  | t <= from     = binarySearchEvents i bs
  | from >= f && to <= t = V.singleton (Event i p)
  | from < f  && to > t  = binarySearchEvents i as <> V.singleton element <> binarySearchEvents i bs
  | from < f  && to <= t = binarySearchEvents i as <> V.singleton (Event (mkInterval f to) p)
  | from >= f && to > t  = V.singleton (Event (mkInterval from t) p) <> binarySearchEvents i bs
  | otherwise = V.empty
  where
    index       = V.length events `quot` 2
    (as, right) = V.splitAt index events
    element     = V.unsafeHead right
    bs          = V.unsafeTail right
    (Event (Interval (f, t)) p) = element

binarySearchIndices :: (Ord t) => Interval t -> V.Vector (Event t p) -> Maybe (Int, Int)
binarySearchIndices interval es = case go interval es of
  (Just left, Just right) -> Just (left, right)
  _ -> Nothing
  where
    go :: (Ord t) => Interval t -> V.Vector (Event t p) -> (Maybe Int, Maybe Int)
    go i@(Interval (from, to)) events
      | V.null events = (Nothing, Nothing)
      | f >= to       = go i as
      | t <= from     = go i bs
      | from >= f && to <= t = (Just index, Just index)
      | from < f  && to > t  = (fst (go i as) `orElse` Just index, snd (go i bs) `orElse` Just index)
      | from < f  && to <= t = (fst (go i as), Just index)
      | from >= f && to > t  = (Just index, snd (go i bs))
      | otherwise = (Nothing, Nothing)
      where
        index       = V.length events `quot` 2
        (as, right) = V.splitAt index events
        element     = V.unsafeHead right
        bs          = V.unsafeTail right
        (Event (Interval (f, t)) _) = element

-----------------------------------------------------------------------------
-- * Updates

-- | /O(n)/. Delete interval from vector of events.
deleteEvents :: Ord t => Interval t -> V.Vector (Event t p) -> V.Vector (Event t p)
deleteEvents interval events = delete' (binarySearchIndices interval events) events
  where
    delete' Nothing es = es
    delete' (Just (left, right)) es = leftPart V.++ rightPart
      where
        leftBound  = sliceEventBound interval (es V.! left)
        rightBound = sliceEventBound interval (es V.! right)
        leftPart   = leftBound `V.cons` V.unsafeInit (fst (V.splitAt left es))
        rightPart  = V.unsafeTail (snd $ V.splitAt right es) `V.snoc` rightBound
        

-----------------------------------------------------------------------------
-- * Extracting sub-timelines.

-- | /O(1)/. Take first /n/ events from timeline.
take :: Int -> Timeline t p -> Timeline t p
take n (Timeline xs) = Timeline (V.take n xs)

-- | /O(1)/. Delete first /n/ events from the timeline.
drop :: Int -> Timeline t p -> Timeline t p
drop n (Timeline xs) = Timeline (V.drop n xs)

-- | /O(n)/. Delete interval from the timeline.
delete :: Ord t => Interval t -> Timeline t p -> Timeline t p
delete i t = Timeline (deleteEvents i (getTimeline t))

-- | /O(1)/. Split timeline on index of event.
splitAtIndex :: Int -> Timeline t p -> (Timeline t p, Timeline t p)
splitAtIndex n (Timeline xs) = (Timeline left, Timeline right)
  where
    (left, right) = V.splitAt n xs

-- | Split timeline on two at given point in time.
-- If given timeline is empty, returns pair of empty timelines.
--
-- >>> let x = fromNaive (" xxxxx" :: PictoralTimeline)
-- >>> splitAtTime 1 x
-- ("", "  xxxxx")
--   
-- >>> let x = fromNaive ("xxxxx" :: PictoralTimeline)
-- >>> splitAtTime 3 x
-- ("xxx", "xx")
splitAtTime :: Ord t => t -> Timeline t p -> (Timeline t p, Timeline t p)
splitAtTime point timeline@(Timeline xs)
  | isEmpty timeline = (empty, empty)
  | otherwise = case splitIndexM of
    Nothing ->
      if unsafeStartTime timeline >= point 
        then (empty, timeline)
        else (timeline, empty)
    Just index -> case middle of
      Left x       -> (Timeline $ V.snoc lefts x, Timeline rights)
      Right (x, y) -> (Timeline $ V.snoc lefts x, Timeline $ V.cons y rights)
      where
        (lefts, rights) = V.splitAt index xs
        middle = splitAt point (unsafeTakeAtIndex index timeline)
  where
    splitIndexM = V.findIndex (includes point) xs  

toChunksOfSize :: Ord t => Int -> Timeline t p -> [Timeline t p]
toChunksOfSize n t
  | isEmpty t = []
  | otherwise = chunk : toChunksOfSize n remaining
  where
    (chunk, remaining) = splitAtIndex n t

shiftWith  
  :: Ord t
  => (t -> t -> t)
  -> t
  -> Timeline t p
  -> Timeline t p
shiftWith f n (Timeline xs) = Timeline (V.map (Data.Timeline.Event.shiftWith f n) xs)

-----------------------------------------------------------------------------
-- * Combinations

unsafeConcat :: Timeline t p -> Timeline t p -> Timeline t p
unsafeConcat (Timeline xs) (Timeline ys) = Timeline (xs V.++ ys)

merge :: Ord t => Timeline t p -> Timeline t p -> Timeline t p
merge a b = Timeline $ mergeEvents (\_ x -> x) (getTimeline a) (getTimeline b)

-- | \( O(n+m) \). Returns timeline union of two timelines. For example,
--
-- >>> let a = "xxx" :: PictoralTimeline
-- >>> let b = "" :: PictoralTimeline
-- >>> mergeW (\a b -> b) (fromNaive a) (fromNaive b)
-- xxx
--
-- >>> let a = "xxx" :: PictoralTimeline
-- >>> let b = "   yyy" :: PictoralTimeline
-- >>> mergeW (\a b -> b) (fromNaive a) (fromNaive b)
-- xxxyyy
--
-- >>> let a = "xxx" :: PictoralTimeline
-- >>> let b = "yyy" :: PictoralTimeline
-- >>> mergeW (\a b -> b) (fromNaive a) (fromNaive b)
-- yyy
--
-- >>> let a = "xxx" :: PictoralTimeline
-- >>> let b = " yyy" :: PictoralTimeline
-- >>> mergeW (\a b -> b) (fromNaive a) (fromNaive b)
-- xyyy
--
-- >>> let a = " xxx" :: PictoralTimeline
-- >>> let b = "yyy" :: PictoralTimeline
-- >>> mergeW (\a b -> b) (fromNaive a) (fromNaive b)
-- yyyx
--
-- >>> let a = "xx" :: PictoralTimeline
-- >>> let b = "   yy" :: PictoralTimeline
-- >>> mergeW (\a b -> b) (fromNaive a) (fromNaive b)
-- xx yy
--
-- >>> let a = " x y z" :: PictoralTimeline
-- >>> let b = "x y z" :: PictoralTimeline
-- >>> mergeW (\a b -> b) (fromNaive a) (fromNaive b)
-- xxyyzz
--
-- >>> let a = "xxxxx" :: PictoralTimeline
-- >>> let b = " a b" :: PictoralTimeline
-- >>> mergeW (\a b -> b) (fromNaive a) (fromNaive b)
-- xaxbx
--
-- >>> let t1 = "xxx yyy zzz"   :: PictoralTimeline
-- >>> let t2 = "  aaa bbb ccc" :: PictoralTimeline
-- >>> mergeW (\a b -> b) (fromNaive t1) (fromNaive t2)
-- xxaaaybbbzccc
--
-- >>> let t1 = "xxxxxxxx"  :: PictoralTimeline
-- >>> let t2 = " aa bb cc" :: PictoralTimeline
-- >>> mergeW (\a b -> b) (fromNaive t1) (fromNaive t2)
-- xaaxbbxcc
--
-- >>> let t1 = " aa bb cc" :: PictoralTimeline
-- >>> let t2 = "xxxxxxxx" :: PictoralTimeline
-- >>> mergeW (\a b -> b) (fromNaive t1) (fromNaive t2)
-- xxxxxxxxc
mergeW :: Ord t => (p -> p -> p) -> Timeline t p -> Timeline t p -> Timeline t p
mergeW f a b = Timeline $ mergeEvents f (getTimeline a) (getTimeline b)

mergeEvents
  :: Ord t
  => (p -> p -> p)
  -> V.Vector (Event t p)
  -> V.Vector (Event t p)
  -> V.Vector (Event t p)
mergeEvents f as bs
  | V.null as && V.null bs = V.empty
  | V.null as = bs
  | V.null bs = as
  | otherwise = mergeEventsImpl f V.empty as bs

mergeEventsImpl
  :: Ord t
  => (p -> p -> p)
  -> V.Vector (Event t p)
  -> V.Vector (Event t p)
  -> V.Vector (Event t p)
  -> V.Vector (Event t p)
mergeEventsImpl f acc as bs
  | V.null as && V.null bs && V.null acc = V.empty
  | V.null as && V.null bs  = acc
  | V.null as && V.null acc = bs
  | V.null bs && V.null acc = as
  | V.null as = mergeEventsImpl f accumulateB as (V.tail bs)
  | V.null bs = mergeEventsImpl f accumulateA (V.tail as) bs
  | otherwise = if getInterval a <= getInterval b
                  then mergeEventsImpl f accumulateA (V.tail as) bs
                  else mergeEventsImpl f accumulateB as (V.tail bs)
  where
    a = V.head as
    b = V.head bs
    accumulateA = if V.null acc
                    then V.singleton a
                    else V.init acc V.++ V.fromList (mergeEventsWith f a (V.last acc))
    accumulateB = if V.null acc
                    then V.singleton b
                    else V.init acc V.++ V.fromList (mergeEventsWith f (V.last acc) b)
                    
intersectWith 
  :: Ord t 
  => (p -> p -> p) 
  -> Timeline t p 
  -> Timeline t p
  -> Timeline t p
intersectWith f x y
  | isEmpty x || isEmpty y = empty
  | otherwise = error "not implemented"
  where
    newBounds = (max (unsafeStartTime x) (unsafeStartTime y), min (unsafeEndTime x) (unsafeEndTime y))
  
                    
-- | Update events using a reference timeline schedule.
-- All of the events from the second timeline are overlayed
-- over events from the first timeline (reference).
-- Gaps from both the original timeline and the reference timeline are preserved.                 
withReference
  :: (Num rel, Ord rel)
  => (abs -> abs -> rel) -- ^ time difference
  -> (abs -> rel -> abs) -- ^ time addition
  -> (a -> b -> c)       -- ^ payload combinator
  -> Timeline abs a      -- ^ reference timeline 
  -> Timeline rel b      -- ^ target timeline
  -> Timeline abs c      -- ^ target timeline overlaid over reference
withReference diff add f as bs = Timeline $ (unsafeIntersectionWithEvent combine . shrink diff) eventsA eventsB
  where
    eventsA = getTimeline as
    eventsB = getTimeline bs
    combine i x y = Event (Interval (from, to)) (f a b)
      where
        from = add f3 (f1 - f2)
        to = add f3 (t1 - f2)
        Interval (f1, t1) = i
        Event (Interval (f2, _)) (Event (Interval (f3, _)) a) = x
        Event _ b = y
          
unsafeIntersectionWithEvent
  :: (Ord t)
  => (Interval t -> Event t a -> Event t b -> Event t' c)
  -> V.Vector (Event t a)
  -> V.Vector (Event t b)
  -> V.Vector (Event t' c)
unsafeIntersectionWithEvent f as bs
  | V.null as = V.empty
  | V.null bs = V.empty 
  | otherwise = go as bs
  where
    go xs ys   
      | r1 < l2   = go (V.tail xs) ys
      | r2 < l1   = go xs (V.tail ys)
      | r1 <= r2  = V.cons z (go (V.tail xs) ys)
      | otherwise = V.cons z (go as (V.tail bs))
      where
        x@(Event (Interval (l1, r1)) _) = V.head xs
        y@(Event (Interval (l2, r2)) _) = V.head ys
        i = mkInterval (max l1 l2) (min r1 r2)
        z = f i x y

-- | /O(N)./ Shrink an (absolute) timeline by removing all the gaps between events.
-- The result is a (relative) timeline with original (absolute) events.
shrink
  :: (Num rel)
  => (abs -> abs -> rel)
  -> V.Vector (Event abs p)
  -> V.Vector (Event rel (Event abs p))
shrink diff events = V.zipWith Event intervals events
  where
    intervals = V.scanl1 step (V.map (toRel diff . getInterval) events)
    step (Interval (_, prevTo)) (Interval (_, dur)) = Interval (prevTo, prevTo + dur) 
       

-----------------------------------------------------------------------------
-- * Conversion
toList
  :: Ord t
  => Timeline t p
  -> [Event t p]
toList (Timeline xs) = V.toList xs 

-- | /O(n)/ Convert Strict structure to Naive
toNaive
  :: Ord t
  => Timeline t p
  -> Naive.Timeline t p
toNaive = Naive.Timeline . toList

