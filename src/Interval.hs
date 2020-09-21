module Interval where

newtype Interval t = Interval (t, t) deriving (Eq, Ord, Show)
