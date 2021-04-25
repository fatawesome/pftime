{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Timeline.Strict where

import           Prelude                hiding (drop, dropWhile, filter, head, last)
import           Data.String            (IsString (..))
import           Data.Generics.Aliases
import           Data.Timeline.Event
import           Data.Timeline.Interval
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
fromEvents events = error "not implemented"

