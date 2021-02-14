-----------------------------------------------------------------------------
-- |
-- Module : Interval
-- Maintainer : fatawesomeee@gmail.com
-- = Interval
--
-- The @'Interval' t@ is a pair of boundaries of type /t/
-----------------------------------------------------------------------------
module Data.Timeline.Interval where

import           Data.Maybe (catMaybes, isJust)
import           Prelude    hiding (length, subtract)

-- $setup
--
-- >>> import Test.QuickCheck
-- >>> import Prelude hiding (subtract)

-----------------------------------------------------------------------------
-- * Interval type

-- | Temporal interval is a pair of points which represent bounded time period.
newtype Interval t = Interval {
  getInterval :: (t, t) -- ^ A pair of points in time.
} deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------
-- * Construction

-- | /O(1)/. If /from/ > /to/, switch them.
--
-- prop> mkInterval 0 1 == Interval (0, 1)
-- prop> mkInterval 1 0 == Interval (0, 1)
mkInterval :: Ord t => t -> t -> Interval t
mkInterval from to
  | from <= to = Interval (from, to)
  | otherwise  = Interval (to, from)

-----------------------------------------------------------------------------
-- * Combine

-- | Intersect two intervals if it is possible.
--
-- prop> intersect (Interval (4,7)) (Interval (5,7)) == Just (Interval (5,7))
-- prop> intersect (Interval (0,1)) (Interval (2,3)) == Nothing
-- prop> intersect (Interval (0,1)) (Interval (1,2)) == Nothing
-- prop> intersect (Interval (0,2)) (Interval (1,3)) == Just (Interval (1,2))
-- prop> intersect (Interval (0,2)) (Interval (0,5)) == Just (Interval (0,2))
-- prop> intersect (Interval (0,5)) (Interval (0,2)) == Just (Interval (0,2))
intersect
  :: Ord t
  => Interval t
  -> Interval t
  -> Maybe (Interval t)
intersect x@(Interval (x1, x2)) y@(Interval (y1, y2))
  | x1 >= y2 || x2 <= y1 = Nothing
  | x1 >= y1 && x2 <= y2 = Just x
  | x1 < y1 && x2 > y2 = Just y
  | x1 >= y1 && x2 > y2 && x1 < y2 = Just (mkInterval x1 y2)
  | x1 <= y1 && x2 <= y2 && x2 > y1 = Just (mkInterval y1 x2)
  | otherwise = Nothing


-- | Creates interval only if given range is valid (i.e. FROM is strictly less than TO)
mkMaybeInterval :: Ord t => (t, t) -> Maybe (Interval t)
mkMaybeInterval (from, to)
  | from < to = Just $ Interval (from, to)
  | otherwise = Nothing

-- | Subtract second argument from first. Works as set difference in terms of type argument.
--
-- prop> subtract (Interval (4,7)) (Interval (5,7)) == [Interval (4,5)]
subtract
  :: Ord t
  => Interval t
  -> Interval t
  -> [Interval t]
subtract x@(Interval (x1, x2)) y
  = case x `intersect` y of
    Just (Interval (i1, i2)) -> catMaybes [mkMaybeInterval (x1, i1), mkMaybeInterval (i2, x2)]
    Nothing -> [x]

concat :: Interval t -> Interval t -> Interval t
concat (Interval a) (Interval b) = Interval (fst a, snd b)

------------------------------------------------------------------------------
-- * Transformations

shiftWith
  :: Ord t
  => (t -> t -> t)
  -> t
  -> Interval t
  -> Interval t
shiftWith f n (Interval (a, b)) = mkInterval (a `f` n) (b `f` n)

------------------------------------------------------------------------------
-- * Properties

-- |
-- prop> adjacent (Interval (1,2)) (Interval (2,3)) == True
-- prop> adjacent (Interval (1,2)) (Interval (4,5)) == False
-- prop> adjacent (Interval (2,3)) (Interval (1,2)) == True
adjacent
  :: Ord t
  => Interval t
  -> Interval t
  -> Bool
adjacent (Interval (a1, a2)) (Interval (b1, b2))
  = a2 == b1 || a1 == b2

intersects
  :: Ord t
  => Interval t
  -> Interval t
  -> Bool
intersects a b
  | isJust (a `intersect` b) = True
  | otherwise                = False
