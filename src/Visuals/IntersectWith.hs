module Visuals.IntersectWith where

import CodeWorld hiding (Event)
import Visuals.Helpers
import Data.Timeline.Event
import Data.Timeline.Interval
import Data.Timeline.Naive

a :: Timeline Int Color
a = Timeline [ Event (mkInterval 0 3) red
             , Event (mkInterval 4 8) red
             , Event (mkInterval 9 12) red
             ]
             
b :: Timeline Int Color
b = Timeline [ Event (mkInterval 2 5) green
             , Event (mkInterval 6 9) green
             , Event (mkInterval 10 13) green
             ]

picA :: Picture
picA = drawPicWithText "A" (drawTimeline a)

picB :: Picture                          
picB = drawPicWithText "B" (drawTimeline b)

mixColors :: Event Int Color -> Event Int Color -> Event Int Color
mixColors (Event i1 color1) (Event _ color2) = Event i1 (mixed [color1, color2]) 
                              
picC :: Picture
picC = drawPicWithText "A intersect B" (drawTimeline $ intersectWith mixColors a b)

pic :: IO()
pic = drawingOf $ drawInColumn [picA, picB, picC]