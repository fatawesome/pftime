{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Timeline.Strict where

import           Prelude                hiding (drop, dropWhile, filter, head, last, splitAt, tail, subtract)
import           Data.String            (IsString (..))
import           GHC.Generics as G hiding (from, to)
import           Data.Generics.Aliases hiding (GT)
import           Data.Timeline.Event
import           Data.Timeline.Interval hiding (overlaps, includes, difference, subtract, Null, One, Two)
import qualified Data.Timeline.Naive    as Naive
import qualified Data.Timeline.Pictoral as Pic (mkPictoralTimeline)
import           Test.QuickCheck           hiding (shrink)
import Control.DeepSeq (NFData)

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

import Control.Monad.ST
import Debug.Trace (trace)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Prelude hiding (take, takeWhile, subtract, null, filter, drop, dropWhile)
-- >>> import Data.Timeline.Pictoral
-- $setup

data Timeline t p = Timeline {
  getTimeline :: !(V.Vector (Event t p))
} deriving (Functor, Eq, G.Generic, NFData)

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

unsafeCons :: Event t p -> Timeline t p -> Timeline t p
unsafeCons x (Timeline xs) = Timeline (V.cons x xs)  

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
-- (, xxxxx)
--   
-- >>> let x = fromNaive ("xxxxx" :: PictoralTimeline)
-- >>> splitAtTime 3 x
-- (xxx,   xx)
splitAtTime :: Ord t => t -> Timeline t p -> (Timeline t p, Timeline t p)
splitAtTime point timeline@(Timeline xs)
  | isEmpty timeline = (empty, empty)
  | otherwise = case splitIndexM of
    Nothing ->
      if unsafeStartTime timeline >= point 
        then (empty, timeline)
        else (timeline, empty)
    Just index -> case middle of
      Left x       -> if end x < point
                        then (timeline, empty)
                        else (empty, timeline)
      Right (x, y) -> (Timeline $ V.snoc lefts x, Timeline $ V.cons y rights)
      where
        (lefts_, rights_) = V.splitAt index xs
        lefts  = if V.null lefts_ then lefts_ else V.init lefts_ 
        rights = if V.null rights_ then rights_ else V.tail rights_
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
                    
merge' 
  :: Ord t
  => Timeline t p
  -> Timeline t p
  -> Timeline t p
merge' = mergeWith' (\_ b -> b)

-- | \( O(n+m) \). Returns timeline union of two timelines. For example,
--
-- >>> let a = "xxx" :: PictoralTimeline
-- >>> let b = "" :: PictoralTimeline
-- >>> mergeWith' (\a b -> b) (fromNaive a) (fromNaive b)
-- xxx
--
-- >>> let a = "xxx" :: PictoralTimeline
-- >>> let b = "   yyy" :: PictoralTimeline
-- >>> mergeWith' (\a b -> b) (fromNaive a) (fromNaive b)
-- xxxyyy
--
-- >>> let a = "xxx" :: PictoralTimeline
-- >>> let b = "yyy" :: PictoralTimeline
-- >>> mergeWith' (\a b -> b) (fromNaive a) (fromNaive b)
-- yyy
--
-- >>> let a = "xxx" :: PictoralTimeline
-- >>> let b = " yyy" :: PictoralTimeline
-- >>> mergeWith' (\a b -> b) (fromNaive a) (fromNaive b)
-- xyyy
--
-- >>> let a = " xxx" :: PictoralTimeline
-- >>> let b = "yyy" :: PictoralTimeline
-- >>> mergeWith' (\a b -> b) (fromNaive a) (fromNaive b)
-- yyyx
--
-- >>> let a = "xx" :: PictoralTimeline
-- >>> let b = "   yy" :: PictoralTimeline
-- >>> mergeWith' (\a b -> b) (fromNaive a) (fromNaive b)
-- xx yy
-- 
-- >>> let a = " x " :: PictoralTimeline
-- >>> let b = "x y" :: PictoralTimeline
-- >>> mergeWith' (\a b -> b) (fromNaive a) (fromNaive b)
-- xxy
-- 
-- >>> let a = " x y" :: PictoralTimeline
-- >>> let b = "x y" :: PictoralTimeline
-- >>> mergeWith' (\a b -> b) (fromNaive a) (fromNaive b)
-- xxyy
--
-- >>> let a = " x y z" :: PictoralTimeline
-- >>> let b = "x y z" :: PictoralTimeline
-- >>> mergeWith' (\a b -> b) (fromNaive a) (fromNaive b)
-- xxyyzz
--
-- >>> let a = "xxxxx" :: PictoralTimeline
-- >>> let b = " a b" :: PictoralTimeline
-- >>> mergeWith' (\a b -> b) (fromNaive a) (fromNaive b)
-- xaxbx
--
-- >>> let t1 = "xxx yyy zzz"   :: PictoralTimeline
-- >>> let t2 = "  aaa bbb ccc" :: PictoralTimeline
-- >>> mergeWith' (\a b -> b) (fromNaive t1) (fromNaive t2)
-- xxaaaybbbzccc
--
-- >>> let t1 = "xxxxxxxx"  :: PictoralTimeline
-- >>> let t2 = " aa bb cc" :: PictoralTimeline
-- >>> mergeWith' (\a b -> b) (fromNaive t1) (fromNaive t2)
-- xaaxbbxcc
--
-- >>> let t1 = " aa bb cc" :: PictoralTimeline
-- >>> let t2 = "xxxxxxxx" :: PictoralTimeline
-- >>> mergeWith' (\a b -> b) (fromNaive t1) (fromNaive t2)
-- xxxxxxxxc
mergeWith' 
  :: forall t p
  . (Ord t)
  => (p -> p -> p)
  -> Timeline t p
  -> Timeline t p
  -> Timeline t p
mergeWith' f as bs
  | isEmpty as && isEmpty bs = empty
  | isEmpty as = bs
  | isEmpty bs = as
  | otherwise = Timeline $ runST $ do 
      result <- M.new ((size as + size bs) * 5)  :: ST s (M.STVector s (Event t p))
      xs <- V.thaw (getTimeline as) :: ST s (M.STVector s (Event t p))
      ys <- V.thaw (getTimeline bs) :: ST s (M.STVector s (Event t p))
      q <- mergeGo f 0 0 0 result xs ys
      let sliced_result = M.unsafeSlice 0 q result
      V.freeze sliced_result

mergeGo
  :: forall t p s
  . (Ord t)
  => (p -> p -> p)
  -> Int -- ^ i counter 
  -> Int -- ^ j counter
  -> Int -- ^ q counter            
  -> M.STVector s (Event t p) -- ^ result
  -> M.STVector s (Event t p) -- ^ as
  -> M.STVector s (Event t p) -- ^ bs
  -> ST s Int
mergeGo f i j q result xs ys = do
  let xl = M.length xs
  let yl = M.length ys

  if i >= xl || j >= yl then do
    if i >= xl && j >= yl then do
      return q
    else do
      if i >= xl then do
        y <- M.read ys j
        resLast <- M.read result (q - 1)
        newEvents <- V.thaw $ V.fromList $ mergeEventsWith f resLast y :: ST s (M.STVector s (Event t p))
        nq <- concatST 0 (q - 1) newEvents result
        mergeGo f i (j + 1) nq result xs ys
      else do
        if j >= yl then do
          x <- M.read xs i
          resLast <- M.read result (q - 1)
          newEvents <- V.thaw $ V.fromList $ mergeEventsWith f x resLast :: ST s (M.STVector s (Event t p))
          nq <- concatST 0 (q - 1) newEvents result
          mergeGo f (i + 1) j nq result xs ys
        else do
          error "Timelines are somehow wrong. Idk, debug it urself. Fuck you."

  else do
    x <- M.read xs i
    y <- M.read ys j

    if start x <= start y then do
      if q == 0 then do
        M.write result q x
        mergeGo f (i + 1) j (q + 1) result xs ys
      else do
        resLast <- M.read result (q - 1)
        newEvents <- V.thaw $ V.fromList $ mergeEventsWith f x resLast :: ST s (M.STVector s (Event t p))
        nq <- concatST 0 (q - 1) newEvents result
        mergeGo f (i + 1) j nq result xs ys
    else do
      if q == 0 then do
        M.write result q y
        mergeGo f i (j + 1) (q + 1) result xs ys
      else do
        resLast <- M.read result (q - 1)
        newEvents <- V.thaw $ V.fromList $ mergeEventsWith f resLast y :: ST s (M.STVector s (Event t p))
        nq <- concatST 0 (q - 1) newEvents result
        mergeGo f i (j + 1) nq result xs ys

concatST 
  :: Int -- ^ counter for events to be inserted 
  -> Int -- ^ current result length 
  -> M.STVector s (Event t p) -- ^ new events
  -> M.STVector s (Event t p) -- ^ result
  -> ST s Int
concatST innerIteractor q newEvents result = do
  let nel = M.length newEvents
  if innerIteractor >= nel then do
    return q
  else do
    boll <- M.read newEvents innerIteractor
    M.write result q boll
    concatST (innerIteractor + 1) (q + 1) newEvents result

intersect
  :: Ord t  
  => Timeline t p 
  -> Timeline t p
  -> Timeline t p
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
intersectWith 
  :: Ord t
  => (p -> p -> p)
  -> Timeline t p
  -> Timeline t p
  -> Timeline t p
intersectWith f xs ys
  | isEmpty xs || isEmpty ys = empty
  | otherwise = if left <= right
      then unsafeCons (eventCreator left right (f (getPayload x) (getPayload y))) next
      else next
  where
    x = unsafeHead xs
    y = unsafeHead ys
    left  = max (start x) (start y)
    right = min (end x) (end y)
    xss = tail xs
    yss = tail ys
    next = if end x < end y
      then intersectWith f xss ys
      else intersectWith f xs yss

intersect'
  :: Ord t
  => Timeline t a
  -> Timeline t b
  -> Timeline t b
intersect' = intersectWith' (\_ b -> b)

-- | /O(N+M)./ Intersect two timelines using conflict resolving function.
--
-- >>> let x = fromNaive ("xxxxx" :: PictoralTimeline)
-- >>> let y = fromNaive (" y y " :: PictoralTimeline)
-- >>> intersectWith' (\a b -> a) x y
--  x x
--
-- >>> let x = fromNaive (" x x " :: PictoralTimeline)
-- >>> let y = fromNaive ("yyyyy" :: PictoralTimeline)
-- >>> intersectWith' (\a b -> a) x y
--  x x
--
-- >>> let x = fromNaive ("xxx " :: PictoralTimeline)
-- >>> let y = fromNaive (" yyy" :: PictoralTimeline)
-- >>> intersectWith' (\a b -> a) x y
--  xx
--
-- >>> let x = fromNaive ("xxx xxx xxx" :: PictoralTimeline)
-- >>> let y = fromNaive ("  yyy yyy  " :: PictoralTimeline)
-- >>> intersectWith' (\a b -> a) x y
--   x x x x
intersectWith'
  :: forall t a b c
  . Ord t
  => (a -> b -> c)
  -> Timeline t a
  -> Timeline t b
  -> Timeline t c
intersectWith' f xs ys
  | isEmpty xs || isEmpty ys = empty
  | otherwise = Timeline $ runST $ do
    as <- V.thaw (getTimeline xs) :: ST s (M.STVector s (Event t a))
    bs <- V.thaw (getTimeline ys) :: ST s (M.STVector s (Event t b))
    result <- M.new (2 * max (size xs) (size ys)) :: ST s (M.STVector s (Event t c))
    q <- intersectGo f 0 0 0 result as bs
    let sliced_result = M.unsafeSlice 0 q result
    V.freeze sliced_result

intersectGo
  :: forall t a b c s
  . Ord t
  => (a -> b -> c)
  -> Int -- ^ i counter
  -> Int -- ^ j counter
  -> Int -- ^ q counter
  -> M.STVector s (Event t c) -- ^ result
  -> M.STVector s (Event t a) -- ^ as
  -> M.STVector s (Event t b) -- ^ bs
  -> ST s Int
intersectGo f i j q result xs ys = do
  let xl = M.length xs
  let yl = M.length ys

  if i >= xl || j >= yl then do
    return q -- if one of vectors is empty, there is nothing to intersect.
  else do
    x <- M.read xs i
    y <- M.read ys j
    let left  = max (start x) (start y)
        right = min (end x) (end y)

    let next = \qPlus -> do if end x < end y
                              then intersectGo f (i + 1) j (q + qPlus) result xs ys
                              else intersectGo f i (j + 1) (q + qPlus) result xs ys

    if left <= right then do
      M.write result q $ eventCreator left right (f (getPayload x) (getPayload y))
      next 1
    else do
      next 0

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
difference
  :: Ord t
  => Timeline t p
  -> Timeline t p
  -> Timeline t p
difference t1 t2
  | isEmpty t1 = empty
  | isEmpty t2 = t1
  | otherwise = go t1 t2
  where
    go a b
      | start x >= end y = difference a ys
      | end x <= start y = unsafeCons x (difference xs b)
      | otherwise = case x `subtract` y of
        Null -> if end x <= end y
                  then difference xs b
                  else difference a ys
        One e -> case end e `compare` end y of
                   LT -> unsafeCons e (difference xs b)
                   EQ -> unsafeCons e (difference xs ys)
                   GT -> difference (unsafeCons e xs) ys 
        Two e1 e2 -> case end e2 `compare` end y of
                       LT -> unsafeConcat (fromList [e1, e2]) (difference xs b)
                       EQ -> unsafeConcat (fromList [e1, e2]) (difference xs ys)
                       GT -> unsafeCons e1 (difference (unsafeCons e2 xs) ys)
      where
        x = unsafeHead a
        y = unsafeHead b
        xs = tail a 
        ys = tail b

-- | /O(N+M)./ Find how first timeline is different from second.
--
-- >>> let t1 = fromNaive ("xxx" :: PictoralTimeline)
-- >>> let t2 = fromNaive ("yyy" :: PictoralTimeline)
-- >>> difference' t1 t2 == empty
-- True
--
-- >>> let t1 = fromNaive ("xxx    " :: PictoralTimeline)
-- >>> let t2 = fromNaive ("    yyy" :: PictoralTimeline)
-- >>> difference' t1 t2
-- xxx
--
-- >>> let t1 = fromNaive ("xxx  " :: PictoralTimeline)
-- >>> let t2 = fromNaive ("  yyy" :: PictoralTimeline)
-- >>> difference' t1 t2
-- xx
--
-- >>> let t1 = fromNaive ("xxx yyy" :: PictoralTimeline)
-- >>> let t2 = fromNaive ("xx   yy" :: PictoralTimeline)
-- >>> difference' t1 t2
--   x y
--
-- >>> let t1 = fromNaive (" yyy" :: PictoralTimeline)
-- >>> let t2 = fromNaive ("yxxx" :: PictoralTimeline)
-- >>> difference' t1 t2 == empty
-- True
--
-- >>> let t1 = fromNaive ("yxxxx" :: PictoralTimeline)
-- >>> let t2 = fromNaive ("yxxx" :: PictoralTimeline)
-- >>> difference' t1 t2
--     x
--
-- >>> let t1 = fromNaive ("xxxx" :: PictoralTimeline)
-- >>> let t2 = fromNaive ("yyxx" :: PictoralTimeline)
-- >>> difference' t1 t2 == empty
-- True
--
-- >>> let t1 = fromNaive ("x x x x " :: PictoralTimeline)
-- >>> let t2 = fromNaive (" y y y y" :: PictoralTimeline)
-- >>> difference' t1 t2
-- x x x x
--
-- >>> let t1 = fromNaive ("xx xx xx xx " :: PictoralTimeline)
-- >>> let t2 = fromNaive (" y  y  y  y" :: PictoralTimeline)
-- >>> difference' t1 t2
-- x  x  x  x
difference'
  :: forall t p
  . Ord t
  => Timeline t p
  -> Timeline t p
  -> Timeline t p
difference' t1 t2
  | isEmpty t1 = empty
  | isEmpty t2 = t1
  | otherwise = Timeline $ runST $ do
    result <- M.new (2 * max (size t1) (size t2)) :: ST s (M.STVector s (Event t p))
    xs <- V.thaw (getTimeline t1) :: ST s (M.STVector s (Event t p))
    ys <- V.thaw (getTimeline t2) :: ST s (M.STVector s (Event t p))
    q <- differenceGo 0 0 0 result xs ys
    let sliced_result = M.unsafeSlice 0 q result
    V.freeze sliced_result

differenceGo
  :: forall t p s
  . Ord t
  => Int -- ^ i counter
  -> Int -- ^ j counter
  -> Int -- ^ q counter
  -> M.STVector s (Event t p) -- ^ result
  -> M.STVector s (Event t p) -- ^ as
  -> M.STVector s (Event t p) -- ^ bs
  -> ST s Int
differenceGo i j q result xs ys = do
  let xl = M.length xs
  let yl = M.length ys

  if i >= xl || j >= yl then do
    if i >= xl then do
      return q
    else do -- if no events in ys, copy the remaining of xs to the result.
      x <- M.read xs i
      M.write result q x
      differenceGo (i + 1) j (q + 1) result xs ys
  else do
    x <- M.read xs i
    y <- M.read ys j

    if start x >= end y then do
      differenceGo i (j + 1) q result xs ys
    else do
      if end x <= start y then do
        M.write result q x
        differenceGo (i + 1) j (q + 1) result xs ys
      else do
        let eventDiff = x `subtract` y
        case eventDiff of
          Null -> if end x <= end y
                    then differenceGo (i + 1) j q result xs ys
                    else differenceGo i (j + 1) q result xs ys
          One e -> case end e `compare` end y of
                     LT -> do
                       M.write result q e
                       differenceGo (i + 1) j (q + 1) result xs ys
                     EQ -> do
                       M.write result q e
                       differenceGo (i + 1) (j + 1) (q + 1) result xs ys
                     GT -> do
                       M.write xs i e
                       differenceGo i (j + 1) q result xs ys
          Two e1 e2 -> case end e2 `compare` end y of
                         LT -> do
                           newEvents <- V.thaw $ V.fromList [e1, e2]
                           nq <- concatST 2 q newEvents result
                           differenceGo (i + 1) j nq result xs ys
                         EQ -> do
                           newEvents <- V.thaw $ V.fromList [e1, e2]
                           nq <- concatST 2 q newEvents result
                           differenceGo (i + 1) (j + 1) nq result xs ys
                         GT -> do
                           M.write result q e1
                           M.write xs i e2
                           differenceGo i (j + 1) (q + 1) result xs ys
                    
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
withReference diff add f as bs = Timeline $ (unsafeIntersectionWithEvent' combine . shrink diff) eventsA eventsB
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

unsafeIntersectionWithEvent'
  :: forall t t' a b c
  . Ord t
  => (Interval t -> Event t a -> Event t b -> Event t' c)
  -> V.Vector (Event t a)
  -> V.Vector (Event t b)
  -> V.Vector (Event t' c)
unsafeIntersectionWithEvent' f t1 t2
  | V.null t1 = V.empty
  | V.null t2 = V.empty 
  | otherwise = runST $ do
    result <- M.new (2 * max (V.length t1) (V.length t2)) :: ST s (M.STVector s (Event t' c))
    as <- V.thaw t1 :: ST s (M.STVector s (Event t a))
    bs <- V.thaw t2 :: ST s (M.STVector s (Event t b))
    q <- go 0 0 0 result as bs
    let sliced_result = M.unsafeSlice 0 q result
    V.freeze sliced_result
  where
    go i j q res xs ys = do
      let xl = M.length xs
      let yl = M.length ys
      
      if i < xl && j < yl then do
        x <- M.read xs i
        y <- M.read ys j
        
        if end x < start y then do
          go (i + 1) j q res xs ys
        else if end y < start x then do
          go i (j + 1) q res xs ys
        else do
          let newInterval = mkInterval (max (start x) (start y)) (min (end x) (end y))
          M.write res q (f newInterval x y)
          if end x <= end y then do
            go (i + 1) j (q + 1) res xs ys
          else do
            go i (j + 1) (q + 1) res xs ys
      else do return q
        
    

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

shrinkTo
  :: (Num rel, Ord rel)
  => rel
  -> (abs -> abs -> rel)
  -> Timeline abs p
  -> Timeline rel (Event abs p)
shrinkTo point diff t = Timeline $ V.zipWith Event intervals events
  where
    events = getTimeline t
    intervals = V.scanl1 step relIntervals
    step (Interval (_, prevTo)) (Interval (_, dur)) = Interval (prevTo, prevTo + dur)
    relIntervals = V.map (mkRelativeTo point . toRel diff . getInterval) events
    mkRelativeTo x = Data.Timeline.Interval.shiftWith (+) x 
       

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

