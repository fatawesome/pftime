module Visuals.Timeline where

import CodeWorld hiding (Event)
import Visuals.Event
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
             
drawTimeline :: Timeline Int Color -> Picture
drawTimeline t = pictures $ map drawEvent (Prelude.reverse $ getTimeline t)

picA :: Picture
picA = drawPicWithText "A" (drawTimeline a)

picB :: Picture                          
picB = drawPicWithText "B" (drawTimeline b) 
                              
picC :: Picture
picC = drawPicWithText "A `union` B" (drawTimeline $ unionBy (\_ x -> x) a b)

unionByPic :: IO()
unionByPic = drawingOf $ drawInColumn [picA, picB, picC]