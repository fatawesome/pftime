module Visuals.NoConflicts where

import CodeWorld hiding (Event)
import Visuals.Event
import Visuals.Helpers
import Data.Timeline.Event
import Data.Timeline.Interval

hasConflictsPic :: Picture
hasConflictsPic = drawEventList [ Event (mkInterval 0 2) red
                                , Event (mkInterval 1 3) blue
                                , Event (mkInterval 2 4) green
                                ]

noConflictsPic :: Picture
noConflictsPic = drawEventList [ Event (mkInterval 0 2) red
                               , Event (mkInterval 2 3) blue
                               , Event (mkInterval 3 4) green
                               ]
                                  
pic :: IO()
pic = drawingOf $ drawInColumn [noConflictsPic, hasConflictsPic]