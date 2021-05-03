module Helpers where
  
import Data.Timeline.Event
import Data.Timeline.Interval

eventCreator :: Ord t => t -> t -> p -> Event t p
eventCreator t t' = Event (mkInterval t t') 

eventCreatorN'withoutOverlapping :: Int -> [Event Int [Char]]
eventCreatorN'withoutOverlapping n 
  | n >= 0 = [eventCreator t (t+1) "SAMPLE_TEXT" | t <- [0 .. n-1] ]
  | otherwise = []

eventCreatorN'withOverlapping :: Int -> [Event Int [Char]]
eventCreatorN'withOverlapping n 
  | n >= 0 = [eventCreator (t `mod` 97) ( (2*t) `mod` 113) "SAMPLE_TEXT" | t <- [0 .. n-1] ]
  | otherwise = []

