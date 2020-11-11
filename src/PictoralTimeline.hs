-----------------------------------------------------------------------------
-- | 
-- Module : PictoralTimeline
-- 
-- = Description
-- Pictoral representation of Timeline structure
-- > mkPictoralTimeline "XXX YYY ZZZ"
-----------------------------------------------------------------------------
module PictoralTimeline where

import Timeline
import Interval

-- | Pictoral representation of Timeline structure.
-- 
-- >>> |-------| 
type PictoralTimeline = Timeline Int Char

-- | Create PictoralTimeline from string.
-- Time is relative to beginning of the string i.e. time (head str) == 0 
mkPictoralTimeline :: String -> PictoralTimeline
mkPictoralTimeline []  = emptyTimeline
mkPictoralTimeline [_] = emptyTimeline
mkPictoralTimeline str = unsafeFromList $ foldr f [] (parse str)
  where
    f el [] = [el]
    f el (x:xs)
      | x `adjacent` el = merge el x : xs
      | otherwise = el : x : xs 

merge :: (Interval t, p) -> (Interval t, p) -> (Interval t, p)
merge (Interval (a1, _), aP) (Interval (_, b2), _)
  = (Interval (a1, b2), aP)

adjacent
  :: (Ord t, Ord a) 
  => (Interval t, a)
  -> (Interval t, a) 
  -> Bool
adjacent (a, aPayload) (b, bPayload) 
  = areAdjacent a b && aPayload == bPayload    

parse :: String -> [(Interval Int, Char)]
parse = filter (\el -> snd el /= ' ') . zipWith (\ i c -> (Interval (i, i + 1), c)) [0 .. ]