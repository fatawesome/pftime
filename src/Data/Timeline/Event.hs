{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Timeline.Event where

import Prelude hiding (splitAt)
import qualified Data.Timeline.Interval as I
import           Control.DeepSeq 
import           GHC.Generics
import           Test.QuickCheck

data Event t p = Event {
  getInterval :: I.Interval t,
  getPayload  :: p
} deriving (Show, Eq, Functor, NFData, Generic)

instance (Ord t, Eq p) => Ord (Event t p) where
  x `compare` y = min (end x) (end y) `compare` max (start x) (start y) 

instance (Ord t, Arbitrary t, Arbitrary p) => Arbitrary (Event t p) where
  arbitrary = Event <$> arbitrary <*> arbitrary

arbitraryEventList :: (Ord t, Arbitrary t, Arbitrary p) => Gen [Event t p]
arbitraryEventList = sized $ \n -> 
  frequency
    [ (1, return [])
    , (n, (:) <$> arbitrary <*> arbitraryEventList)]

-- | /O(1)/.
--
-- prop> fromTuple (I.Interval (1,2), 'a') == Event (I.Interval (1,2)) 'a'
fromTuple :: (I.Interval t, p) -> Event t p
fromTuple (i, p) = Event i p

fromTriple :: p -> t -> t -> Event t p
fromTriple p f t = Event (I.Interval (f, t)) p

start :: Event t p -> t
start (Event (I.Interval (f, _)) _) = f
  
end :: Event t p -> t
end (Event (I.Interval (_, t)) _) = t

-- | Convert Event to tuple.
toTuple :: Event t p -> (t, t, p)
toTuple (Event (I.Interval (f, t)) p) = (f, t, p)

changeFrom :: Ord t => t -> Event t p -> Event t p
changeFrom from (Event (I.Interval (_, to)) payload) = Event (I.mkInterval from to) payload

changeTo :: Ord t => t -> Event t p -> Event t p
changeTo to (Event (I.Interval (from, _)) payload) = Event (I.mkInterval from to) payload

sliceEventBound :: Ord t => I.Interval t -> Event t p -> Event t p
sliceEventBound interval (Event oldInterval payload) = Event (I.sliceBound interval oldInterval) payload

shiftWith
  :: Ord t
  => (t -> t -> t)
  -> t
  -> Event t p
  -> Event t p
shiftWith f n (Event i p) = Event (I.shiftWith f n i) p

-- |
--
-- prop> mergeEventsWith (\a b -> b) (Event (I.mkInterval 0 3) 'x') (Event (I.mkInterval 3 6) 'y') == [(Event (I.mkInterval 0 3) 'x'), (Event (I.mkInterval 3 6) 'y')]
-- prop> mergeEventsWith (\a b -> b) (Event (I.mkInterval 0 3) 'x') (Event (I.mkInterval 1 4) 'y') == [(Event (I.mkInterval 0 1) 'x'), (Event (I.mkInterval 1 3) 'y'), (Event (I.mkInterval 3 4) 'y')]
-- prop> mergeEventsWith (\a b -> b) (Event (I.mkInterval 0 2) 'x') (Event (I.mkInterval 0 1) 'y') == [(Event (I.mkInterval 0 1) 'y'), (Event (I.mkInterval 1 2) 'x')]
-- prop> mergeEventsWith (\a b -> b) (Event (I.mkInterval 0 1) 'x') (Event (I.mkInterval 0 2) 'y') == [(Event (I.mkInterval 0 1) 'y'), (Event (I.mkInterval 1 2) 'y')]
mergeEventsWith
  :: Ord t
  => (p -> p -> p)
  -> Event t p
  -> Event t p
  -> [Event t p]
mergeEventsWith f x@(Event (I.Interval (x1, x2)) xp) y@(Event (I.Interval (y1, y2)) yp)
  -- xxx
  --    yyy
  | x2 <= y1 = [x, y]

  --    xxx
  -- yyy
  | x1 >= y2 = [y, x]
  | x1 == y1 && x2 == y2 = [Event (I.mkInterval x1 x2) (f xp yp)]

  -- xxx
  -- yyyy
  | x1 == y1 && x2 < y2 = [ Event (I.mkInterval x1 x2) (f xp yp)
                          , Event (I.mkInterval x2 y2) yp
                          ]

  --  xxx
  -- yyyy
  | x1 > y1 && x2 == y2 = [ Event (I.mkInterval y1 x1) yp
                          , Event (I.mkInterval x1 x2) (f xp yp)
                          ]

  -- xxxx
  -- yyy
  | x1 == y1 && x2 > y2 = [ Event (I.mkInterval x1 y2) (f xp yp)
                          , Event (I.mkInterval y2 x2) xp
                          ]

  -- xxxx
  --  yyy
  | x1 < y1 && x2 == y2 = [ Event (I.mkInterval x1 y1) xp
                          , Event (I.mkInterval y1 x2) (f xp yp)
                          ]

  -- xxxxx
  --  yyy
  | x1 < y1 && x2 > y2 = [ Event (I.mkInterval x1 y1) xp
                         , Event (I.mkInterval y1 y2) (f xp yp)
                         , Event (I.mkInterval y2 x2) xp
                         ]
  --  xxx
  -- yyyyy
  | x1 > y1 && x2 < y2 = [ Event (I.mkInterval y1 x1) yp
                         , Event (I.mkInterval x1 x2) (f xp yp)
                         , Event (I.mkInterval x2 y2) yp
                         ]
  -- xxx
  --  yyy
  | x1 < y1 && x2 < y2 = [ Event (I.mkInterval x1 y1) xp
                         , Event (I.mkInterval y1 x2) (f xp yp)
                         , Event (I.mkInterval x2 y2) yp
                         ]
  --  xxx
  -- yyy
  | x1 > y1 && x2 > y2 = [ Event (I.mkInterval y1 x1) yp
                         , Event (I.mkInterval x1 y2) (f xp yp)
                         , Event (I.mkInterval y2 x2) xp
                         ]

  -- invalid cases
  | otherwise = []


-- | /O(1)/. Events are adjancent if one strictly follows other and their payloads are equal.
--
-- >>> e1 = Event (I.Interval (1,2)) 'a'
-- >>> e2 = Event (I.Interval (2,3)) 'a'
-- >>> e1 `adjacent` e2
-- True
adjacent
  :: (Ord t, Eq p)
  => Event t p
  -> Event t p
  -> Bool
adjacent e1 e2
  = I.adjacent (getInterval e1) (getInterval e2) && getPayload e1 == getPayload e2


eventCreator :: Ord t => t -> t -> p -> Event t p
eventCreator t t' = Event (I.mkInterval t t') 

eventListWithoutOverlapsOfLength :: Int -> [Event Int [Char]]
eventListWithoutOverlapsOfLength n 
  | n >= 0 = [eventCreator t (t+1) "SAMPLE_TEXT" | t <- [0 .. n-1] ]
  | otherwise = []

eventCreatorN'withOverlapping :: Int -> [Event Int [Char]]
eventCreatorN'withOverlapping n 
  | n >= 0 = [eventCreator (t `mod` 97) ( (2*t) `mod` 113) "SAMPLE_TEXT" | t <- [0 .. n-1] ]
  | otherwise = []
  
includes :: Ord t => t -> Event t p -> Bool
includes point (Event interval _) = interval `I.includes` point

splitAt 
  :: Ord t 
  => t 
  -> Event t p 
  -> Either (Event t p) (Event t p, Event t p)
splitAt point event@(Event (I.Interval (l, r)) payload)
  | point <= l || r <= point = Left event
  | otherwise = Right (eventCreator l point payload, eventCreator point r payload)

