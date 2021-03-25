module Visuals.Ascending where

import CodeWorld hiding (Event)
import Visuals.Event
import Visuals.Helpers
import Data.Timeline.Event
import Data.Timeline.Interval

isAscendingGoodPic :: Picture
isAscendingGoodPic = drawEventList [ Event (mkInterval 0 2) red
                                   , Event (mkInterval 2 4) blue
                                   , Event (mkInterval 4 6) green
                                   ]

isAscendingBadPic :: Picture
isAscendingBadPic = drawEventList [ Event (mkInterval 2 4) red
                                  , Event (mkInterval 0 2) blue
                                  , Event (mkInterval 4 6) green
                                  ]
                                  
pic :: IO()
pic = drawingOf $ drawInColumn [isAscendingGoodPic, isAscendingBadPic]