module Interval where

newtype Interval t = Interval (t, t)
  deriving (Eq, Ord, Show)

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
  | x1 >= y1 && x2 > y2 = Just (mkInterval (x1, y2))
  | x1 <= y1 && x2 < y2 = Just (mkInterval (y1, x2))
  | otherwise = Nothing
  
--subtract
--  :: Ord t
--  => Interval t
--  -> Interval t
--  -> Maybe [Interval t]
--subtract x@(Interval (x1, x2)) y@(Interval (y1, y2))
--  = case intersectIntervals x y of
--    Just (Interval (i1, i2)) -> []
--    Nothing -> Nothing
