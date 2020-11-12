-----------------------------------------------------------------------------
-- |
-- Module : Interval
-- Maintainer : fatawesomeee@gmail.com
-- = Interval
--
-- The @'Interval' t@ is a pair of boundaries of type /t/ 
-----------------------------------------------------------------------------

module Interval where

import Data.Maybe (catMaybes)

-- $setup
--
-- >>> import Test.QuickCheck
-- >>> instance (Ord t, Arbitrary t) => Arbitrary (Interval t) where arbitrary = mkInterval <$> arbitrary

-----------------------------------------------------------------------------
-- * Interval type

-- | Temporal interval is a pair of points which represent bounded time period. 
-- prop> fst (getInterval i) < snd (getInterval i)
newtype Interval t = Interval {
  getInterval :: (t, t) -- ^ A pair of points in time.
} deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------
-- * Construction

-- | /O(1)/. If /from/ > /to/, switch them.
mkInterval :: Ord t => (t, t) -> Interval t
mkInterval (from, to)
  | from <= to = Interval (from, to)
  | otherwise  = Interval (to, from)

intersectIntervals
  :: Ord t 
  => Interval t
  -> Interval t
  -> Maybe (Interval t)
intersectIntervals x@(Interval (x1, x2)) y@(Interval (y1, y2))
  | x1 >= y1 && x2 <= y2 = Just x
  | x1 < y1 && x2 > y2 = Just y
  | x1 >= y1 && x2 > y2 && x1 < y2 = Just (mkInterval (x1, y2))
  | x1 <= y1 && x2 < y2 && x2 > y1 = Just (mkInterval (y1, x2))
  | otherwise = Nothing
  

-- | Creates interval only if given range is valid (i.e. FROM is strictly less than TO)
mkMaybeInterval :: Ord t => (t, t) -> Maybe (Interval t)
mkMaybeInterval (from, to)
  | from < to = Just $ Interval (from, to)
  | otherwise = Nothing
  
-- | Subtract second argument from first. Works as set difference in terms of type argument.
subtractInterval
  :: Ord t
  => Interval t
  -> Interval t
  -> [Interval t]
subtractInterval x@(Interval (x1, x2)) y
  = case intersectIntervals x y of
    Just (Interval (i1, i2)) -> catMaybes [mkMaybeInterval (x1, i1), mkMaybeInterval (i2, x2)] 
    Nothing -> [x]
    
concat :: Interval t -> Interval t -> Interval t
concat (Interval a) (Interval b) = Interval (fst a, snd b)

areAdjacent :: Ord t => Interval t -> Interval t -> Bool
areAdjacent (Interval (a1, a2)) (Interval (b1, b2))
  = a2 == b1 || a1 == b2