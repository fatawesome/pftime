module Visuals.WithReference where

import CodeWorld hiding (Event)
import Visuals.Helpers
import Data.Timeline.Event
import Data.Timeline.Interval
import Data.Timeline.Naive

a :: Timeline Int Color
a = Timeline [ Event (mkInterval 2 4) black
             , Event (mkInterval 5 7) black
             , Event (mkInterval 8 10) black
             ]

b :: Timeline Int Color
b = Timeline [ Event (mkInterval 0 1) red
             , Event (mkInterval 1 2) orange
             , Event (mkInterval 2 3) yellow
             , Event (mkInterval 3 4) green
             , Event (mkInterval 4 5) blue
             , Event (mkInterval 5 6) purple
             ]

picA :: Picture
picA = drawPicWithText "A" (drawTimeline a)

picB :: Picture
picB = drawPicWithText "B" (drawTimeline b) <> translated 0 (-0.7) (numbers 7)

picC :: Picture
picC = drawPicWithText "A `withReference` B" (drawTimeline $ a `withReference_` b)

pic :: IO()
pic = drawingOf $ drawInColumn [picA, picB, picC]