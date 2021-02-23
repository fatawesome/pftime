module Visuals.Relations where

import CodeWorld hiding (Event)
import Visuals.Event
import Visuals.Helpers
import Data.Timeline.Interval
import Data.Timeline.Event

beforePic :: Picture
beforePic = drawEventsWithText "Before" [Event (mkInterval 0 2) red, Event (mkInterval 3 5) blue]

equalPic :: Picture
equalPic = drawEventsWithText "Equal" [Event (mkInterval 0 3) red, Event (mkInterval 0 3) blue]

meetsPic :: Picture
meetsPic = drawEventsWithText "Meets" [Event (mkInterval 0 5) red, Event (mkInterval 1 3) blue]

overlapsPic :: Picture
overlapsPic = drawEventsWithText "Overlaps" [Event (mkInterval 0 3) red, Event (mkInterval 1 4) blue]

duringPic :: Picture
duringPic = drawEventsWithText "During" [Event (mkInterval 1 3) red, Event (mkInterval 0 5) blue]

startsPic :: Picture
startsPic = drawEventsWithText "Starts" [Event (mkInterval 0 3) red, Event (mkInterval 0 5) blue]

finishesPic :: Picture
finishesPic = drawEventsWithText "Finishes" [Event (mkInterval 2 5) red, Event (mkInterval 0 5) blue]

pic :: IO()
pic = drawingOf $ drawInColumn [beforePic, equalPic, meetsPic, overlapsPic, duringPic, startsPic, finishesPic] 