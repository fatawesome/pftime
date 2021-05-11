{-# LANGUAGE GeneralisedNewtypeDeriving #-}
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
import Control.DeepSeq

-- $setup
--
-- >>> import Test.QuickCheck
-- >>> import Prelude hiding (subtract)

-----------------------------------------------------------------------------
-- * Interval type

-- | Temporal interval is a pair of points which represent bounded time period.
newtype Interval t = Interval (t, t) deriving (Eq, Ord, Show, NFData)

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
  
-- | Function removes left OR right part of interval.
-- Otherwise (if slicing returns list of intervals) throws an error.
sliceBound :: Ord t => Interval t -> Interval t -> Interval t
sliceBound x y = case subtract x y of
  [interval] -> interval
  _          -> error "Error: subtract to single can be used only with confidence that operation will return 1 exact interval" 
   

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

-- | Convert numerical interval from absolute to relative time.
toRel :: (Num rel) => (abs -> abs -> rel) -> Interval abs -> Interval rel 
toRel diff (Interval (from, to)) = Interval (0, to `diff` from)

------------------------------------------------------------------------------
-- * Relationships

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


-- Thirteen possible relationships between temporal intervals introduced by Allen.

before :: Ord t => Interval t -> Interval t -> Bool
before (Interval (_, t1)) (Interval (f2, _)) = t1 <= f2

equal :: Ord t => Interval t -> Interval t -> Bool
equal = (==)

meets :: Ord t => Interval t -> Interval t -> Bool
meets (Interval (f1, t1)) (Interval (f2, t2)) = f1 <= f2 && t1 >= t2

overlaps :: Ord t => Interval t -> Interval t -> Bool
overlaps (Interval (f1, t1)) (Interval (f2, t2)) = t1 > f2 && f1 < t2

during :: Ord t => Interval t -> Interval t -> Bool
during = flip meets

starts :: Ord t => Interval t -> Interval t -> Bool
starts (Interval (f1, _)) (Interval (f2, _)) = f1 == f2

finishes :: Ord t => Interval t -> Interval t -> Bool
finishes (Interval (_, t1)) (Interval (_, t2)) = t1 == t2