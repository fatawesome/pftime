{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
module Data.Timeline.Pictoral where

import           Data.String            (IsString (..))

import           Data.Timeline.Event    as Event
import           Data.Timeline.Interval as Interval
import           Data.Timeline.Naive    hiding (filter)

-----------------------------------------------------------------------------
-- * Pictoral timeline type

type PictoralTimeline = Timeline Int Char

instance IsString PictoralTimeline where
  fromString = mkPictoralTimeline

instance {-# OVERLAPPING #-} Integral t => Show (Timeline t Char) where
  show = toString

-----------------------------------------------------------------------------
-- * Construction

-- | Create PictoralTimeline from string.
-- Time is relative to beginning of the string i.e. time (head str) == 0
mkPictoralTimeline :: (Ord t, Num t) => String -> Timeline t Char
mkPictoralTimeline []  = empty
mkPictoralTimeline str = unsafeFromList $ foldr f [] (parse str)
  where
    f el [] = [el]
    f el (x:xs)
      | Event.adjacent x el = mergeAdjacentEvents el x : xs
      | otherwise = el : x : xs

-- * Representation

toString :: Integral t => Timeline t Char -> String
toString (Timeline tl) =
  case tl of
    []                                  -> ""
    e@(Event (Interval (from, _)) _):es -> replicate (fromIntegral from) ' ' 
                                           <> eventToString e 
                                           <> eventsToString e es
  where
    eventToString (Event (Interval (from, to)) c) = replicate (fromIntegral (to - from)) c
    eventsToString e es = Prelude.concat
      [ replicate (fromIntegral (from2 - to1)) ' ' <> eventToString e2
      | (e1, e2) <- zip (e:es) es
      , let Event (Interval (_, to1)) _ = e1
      , let Event (Interval (from2, _)) _ = e2
      ]

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

parse :: Num t => String -> [Event t Char]
parse s =
  [ Event.fromTuple (Interval (i, i + 1), c)
  | (i', c) <- zip [0..] s
  , c /= ' '
  , let i = fromIntegral i'
  ]

toStringImpl :: (String, [Event Int Char]) -> (String, [Event Int Char])
toStringImpl (string, []) = (string, [])
toStringImpl (string, x@(Event (Interval (left, right)) char) : xs)
  | length string < left = toStringImpl (string ++ emptiness (left - length string), x:xs)
  | otherwise = toStringImpl (string ++ replicate (right - left) char, xs)
