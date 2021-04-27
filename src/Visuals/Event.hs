module Visuals.Event where

import Data.Text as Text
import CodeWorld hiding (Event)
import Visuals.Interval
import Data.Timeline.Interval
import Data.Timeline.Event

drawEvent :: Event Int Color -> Picture
drawEvent (Event i color) = colored color (drawInterval i)

drawEventList :: [Event Int Color] -> Picture
drawEventList = drawEventListAt 0

drawEventListAt :: Double -> [Event Int Color] -> Picture
drawEventListAt _     [] = blank
drawEventListAt start (x:xs) 
  = translated 0 start (drawEvent x) <> drawEventListAt (start - boundaryHeight - 0.1) xs

drawEventsWithText :: String -> [Event Int Color] -> Picture
drawEventsWithText str events
  = translated (-2) 0 (dilated 0.5 $ lettering (Text.pack str))
      <> drawEventList events

example :: IO()
example = drawingOf $ drawEventList [ Event (mkInterval 0 3) red
                                    , Event (mkInterval 2 6) yellow
                                    , Event (mkInterval 4 7) green
                                    , Event (mkInterval 6 10) black
                                    ]