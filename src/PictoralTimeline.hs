-----------------------------------------------------------------------------
-- | 
-- Module : PictoralTimeline
-- 
-- = Description
-- Pictoral representation of Timeline structure.
-- For example, timeline "XXX YYY ZZZ"
-- >>> mkPictoralTimeline "XXX YYY ZZZ"
-- Timeline {getTimeline = [(Interval {getInterval = (0,3)},'X'),(Interval {getInterval = (4,7)},'Y'),(Interval {getInterval = (8,11)},'Z')]}
-----------------------------------------------------------------------------

module PictoralTimeline (
  PictoralTimeline,
  mkPictoralTimeline,
  toString
) where

import Timeline
import Interval

-----------------------------------------------------------------------------
-- * Pictoral timeline type

type PictoralTimeline = Timeline Int Char

-----------------------------------------------------------------------------
-- * Construction

-- | Create PictoralTimeline from string.
-- Time is relative to beginning of the string i.e. time (head str) == 0 
mkPictoralTimeline :: String -> PictoralTimeline
mkPictoralTimeline []  = empty
mkPictoralTimeline [_] = empty
mkPictoralTimeline str = unsafeFromList $ foldr f [] (parse str)
  where
    f el [] = [el]
    f el (x:xs)
      | areAdjacentWithPayload x el = mergeAdjacentIntervals el x : xs
      | otherwise = el : x : xs
      
-- * Representation

toString :: PictoralTimeline -> String
toString (Timeline []) = ""
toString (Timeline xs) = result 
  where
    start = replicate (fst $ getInterval $ fst (head xs)) ' '
    (result, _) = helper (start, xs) 

helper :: (String, [(Interval Int, Char)]) -> (String, [(Interval Int, Char)])
helper (string, []) = (string, [])
helper (string, x@(Interval (left, right), char) : xs)
  | length string < left = helper (string ++ emptiness (left - length string), x:xs)
  | otherwise = helper (string ++ replicate (right - left) char, xs)  
  
emptiness 
  :: Int    -- ^ Length of empty space 
  -> String -- ^ String with N spaces
emptiness n = replicate n ' '

-----------------------------------------------------------------------------
-- * Helpers
-- = WARNING
-- Following functions are meant to be used only in this module

mergeAdjacentIntervals :: (Interval t, p) -> (Interval t, p) -> (Interval t, p)
mergeAdjacentIntervals (Interval (a1, _), aP) (Interval (_, b2), _)
  = (Interval (a1, b2), aP)

areAdjacentWithPayload
  :: (Ord t, Ord a) 
  => (Interval t, a)
  -> (Interval t, a) 
  -> Bool
areAdjacentWithPayload (a, aPayload) (b, bPayload)
  = areAdjacent a b && aPayload == bPayload    


parse :: String -> [(Interval Int, Char)]
parse = filter (\el -> snd el /= ' ') . zipWith (\ i c -> (Interval (i, i + 1), c)) [0 .. ]