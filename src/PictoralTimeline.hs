-----------------------------------------------------------------------------
-- | 
-- Module : PictoralTimeline
-- 
-- = Description
-- Pictoral representation of Timeline structure.
-- For example, timeline "XXX YYY ZZZ"
-- > mkPictoralTimeline "XXX YYY ZZZ"
-- Timeline [Event (Interval (0,3)) 'X'}, Event (Interval (4,7)) 'Y'}, Event (Interval (8,11)) 'Z'}]
-----------------------------------------------------------------------------

module PictoralTimeline (
  PictoralTimeline,
  mkPictoralTimeline,
  toString
) where

import Timeline
import Interval
import Event

-----------------------------------------------------------------------------
-- * Pictoral timeline type

type PictoralTimeline = Timeline Int Char

-----------------------------------------------------------------------------
-- * Construction

-- | Create PictoralTimeline from string.
-- Time is relative to beginning of the string i.e. time (head str) == 0 
mkPictoralTimeline :: String -> PictoralTimeline
mkPictoralTimeline []  = empty
mkPictoralTimeline str = unsafeFromList $ foldr f [] (parse str)
  where
    f el [] = [el]
    f el (x:xs)
      | Event.adjacent x el = mergeAdjacentEvents el x : xs
      | otherwise = el : x : xs

-- * Representation

toString :: PictoralTimeline -> String
toString (Timeline []) = ""
toString (Timeline xs) = result
  where
    start = replicate (fst $ getInterval $ interval (head xs)) ' '
    (result, _) = helper (start, xs)

helper :: (String, [Event Int Char]) -> (String, [Event Int Char])
helper (string, []) = (string, [])
helper (string, x@(Event (Interval (left, right)) char) : xs)
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

mergeAdjacentEvents :: Event t p -> Event t p -> Event t p
mergeAdjacentEvents (Event (Interval (a1, _)) aP) (Event (Interval (_, b2)) _)
  = Event (Interval (a1, b2)) aP

parse :: String -> [Event Int Char]
parse s = map Event.fromTuple (tuples s)
  where
    tuples str = filter (\el -> snd el /= ' ') (zipWith (\ i c -> (Interval (i, i + 1), c)) [0 .. ] str)