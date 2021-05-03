{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Timeline.Strict where

import           Prelude                hiding (drop, dropWhile, filter, head, last)
import           Data.String            (IsString (..))
import           Data.Generics.Aliases
import           Data.Timeline.Event
import           Data.Timeline.Interval hiding (getInterval)
import qualified Data.Timeline.Naive    as Naive
import qualified Data.Timeline.Pictoral as Pic (mkPictoralTimeline)
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

instance (Ord t, Num t, Integral t) => IsString (Timeline t Char) where
  fromString = fromNaive . Pic.mkPictoralTimeline

instance Integral t => Show (Timeline t Char) where
  show = show . toNaive

-----------------------------------------------------------------------------
-- * Accessors

isEmpty :: Timeline t p -> Bool
isEmpty (Timeline ps froms tos) = V.null ps || V.null froms || V.null tos

head :: Timeline t p -> Maybe (Event t p)
head t@(Timeline ps fs ts)
  | isEmpty t = Nothing
  | otherwise = Just (Event (Interval (V.head fs, V.head ts)) (V.head ps))

last :: Timeline t p -> Maybe (Event t p)
last t@(Timeline ps fs ts)
  | isEmpty t = Nothing
  | otherwise = Just (Event (Interval (V.last fs, V.last ts)) (V.last ps))

unsafeHead :: Timeline t p -> Event t p
unsafeHead (Timeline ps fs ts) = Event (Interval (V.head fs, V.head ts)) (V.head ps)

unsafeLast :: Timeline t p -> Event t p
unsafeLast (Timeline ps fs ts) = Event (Interval (V.last fs, V.last ts)) (V.last ps)

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
  
unsafeFromEvents :: V.Vector (Event t p) -> Timeline t p
unsafeFromEvents = error "not implemented"
  where
    event (Event (Interval (l, r)) p) = (p, l, r) 

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
filterEvents
  :: (Event t p -> Bool)
  -> Timeline t p
  -> Timeline t p
filterEvents f (Timeline ps fs ts) = fromVectorsTuple $ V.unzip3 $ V.filter g (V.zip3 ps fs ts)
  where
    g = f . uncurry3 fromTriple
    fromVectorsTuple (a, b, c) = Timeline a b c

-- | /O(log(n)/ Search for events which happen during given time interval.
-- If there are no such events, returns empty timeline.
search :: Ord t => Interval t -> Timeline t p -> Timeline t p
search interval t = fromEvents $ binarySearchEvents interval (toEvents t)

--searchIndices :: Ord t => Interval t -> Timeline t p -> (Int, Int)


-- | /O(log(n))/ Binary search in vector with entries of Event type.
--
-- >>> let t = "aaaaa" :: PictoralTimeline
-- >>> binarySearchEvents (mkInterval 1 3) (toEvents $ fromNaive t) == V.singleton (Event (mkInterval 1 3) 'a')
-- True
--
-- >>> let t = "aaaaa" :: PictoralTimeline
-- >>> binarySearchEvents (mkInterval 0 7) (toEvents $ fromNaive t) == V.singleton (Event (mkInterval 0 5) 'a')
-- True
--
-- >>> let t = "  aaa" :: PictoralTimeline
-- >>> binarySearchEvents (mkInterval 0 4) (toEvents $ fromNaive t) == V.singleton (Event (mkInterval 2 4) 'a')
-- True
--
-- >>> let t = "aaa bbb" :: PictoralTimeline
-- >>> binarySearchEvents (mkInterval 0 7) (toEvents $ fromNaive t) == V.fromList [(Event (mkInterval 0 3) 'a'), (Event (mkInterval 4 7) 'b')]
-- True
--
-- >>> let t = "aaa bbb" :: PictoralTimeline
-- >>> binarySearchEvents (mkInterval 3 4) (toEvents $ fromNaive t) == V.empty
-- True
--
-- >>> let t = "aaa bbb" :: PictoralTimeline
-- >>> binarySearchEvents (mkInterval 2 5) (toEvents $ fromNaive t) == V.fromList [(Event (mkInterval 2 3) 'a'), (Event (mkInterval 4 5) 'b')]
-- True
--
-- >>> let t = "aaa bbb ccc" :: PictoralTimeline
-- >>> binarySearchEvents (mkInterval 2 9) (toEvents $ fromNaive t) == V.fromList [(Event (mkInterval 2 3) 'a'), (Event (mkInterval 4 7) 'b'), (Event (mkInterval 8 9) 'c')]
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

-- | /O(1)/ Delete first /n/ events from the timeline.
drop
  :: Int
  -> Timeline t p
  -> Timeline t p
drop n (Timeline ps fs ts) = Timeline (V.drop n ps) (V.drop n fs) (V.drop n ts)

-- | /O(n)/. Delete interval from the timeline.
delete :: Ord t => Interval t -> Timeline t p -> Timeline t p
delete i t = fromEvents $ deleteEvents i (toEvents t)

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
-- * Combinations

unsafeConcat :: Timeline t p -> Timeline t p -> Timeline t p
unsafeConcat (Timeline ps1 fs1 ts1) (Timeline ps2 fs2 ts2)
  = Timeline (ps1 V.++ ps2) (fs1 V.++ fs2) (ts1 V.++ ts2)

merge :: Ord t => Timeline t p -> Timeline t p -> Timeline t p
merge a b = fromEvents $ mergeEvents (\_ x -> x) (toEvents a) (toEvents b)

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
mergeW f a b = fromEvents $ mergeEvents f (toEvents a) (toEvents b)

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
                    else V.init acc V.++ V.fromList (mergeWith f a (V.last acc))
    accumulateB = if V.null acc
                    then V.singleton b
                    else V.init acc V.++ V.fromList (mergeWith f (V.last acc) b)
                    
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
withReference diff add f as bs = fromEvents $ (unsafeIntersectionWithEvent combine . shrink diff) eventsA eventsB
  where
    eventsA = toEvents as
    eventsB = toEvents bs
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
-- | /O(n)/ Convert Strict structure to Naive
toNaive
  :: Ord t
  => Timeline t p
  -> Naive.Timeline t p
toNaive (Timeline ps froms tos) = Naive.Timeline $ V.toList $ V.zipWith3 toNaiveEvent ps froms tos
  where
    toNaiveEvent p from to = Event (mkInterval from to) p

toEvents :: Timeline t p -> V.Vector (Event t p)
toEvents (Timeline ps fs ts) = V.map (uncurry3 fromTriple) (V.zip3 ps fs ts)

fromEvents :: V.Vector (Event t p) -> Timeline t p
fromEvents events = fromNaive $ Naive.Timeline (V.toList events)

