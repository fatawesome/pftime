module Interval where

newtype Interval t = Interval (t, t)
  deriving (Eq, Ord, Show)

mkInterval :: Ord t => (t, t) -> Interval t
mkInterval (from, to)
  | from <= to = Interval (from, to)
  | otherwise  = Interval (to, from)
