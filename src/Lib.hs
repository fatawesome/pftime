{-# LANGUAGE GADTs #-}

module Lib
  ( someFunc,
  )
where

import Data.Monoid
import Data.Semigroup

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Interval t e = Interval (t, t) e

newtype Timeline t e = Timeline [Interval t e]

instance Semigroup (Timeline t e) where
  Timeline x <> Timeline y = Timeline (x <> y)

instance Monoid (Timeline t e) where
  mempty = Timeline []

orderIntervals ::
  Ord t =>
  Interval t e ->
  Interval t e ->
  Maybe ((Interval t e), (Interval t e))
orderIntervals (startLeft, endLeft) payloadLeft (startRight, endRight) payloadRight
  | (startLeft < startRight) && (endLeft < endRight) =
    Just (Interval ((startLeft, endLeft) payloadLeft) Interval ((startRight, endRight) payloadRight))

mergeIntervals ::
  Ord t =>
  (Interval t e -> Interval t e -> Interval t e) ->
  Interval t e ->
  Interval t e ->
  Interval t e
mergeIntervals resolveConflict i1 i2 =
  _

concatTimelines ::
  Ord t =>
  Timeline t e ->
  Timeline t e ->
  (Interval t e -> Interval t e -> Interval t e) ->
  Timeline t e
concatTimelines (Timeline intervals1) (Timeline intervals2) resolveConflict =
  Timeline (zipWith (mergeIntervals resolveConflict) intervals1 intervals2)
