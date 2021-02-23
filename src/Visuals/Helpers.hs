module Visuals.Helpers where

import CodeWorld
import Data.Text as Text
import Data.Timeline.Naive
import Visuals.Event

drawInColumn :: [Picture] -> Picture
drawInColumn = drawAllFrom 0

drawAllFrom :: Double -> [Picture] -> Picture
drawAllFrom _     []   = blank
drawAllFrom start (p:ps) = translated 0 start p <> drawAllFrom (start - 2) ps

drawPicWithText :: String -> Picture -> Picture
drawPicWithText str pic 
  = translated (-2) 0 (dilated 0.5 $ lettering (Text.pack str)) <> pic  
  
drawTimeline :: Timeline Int Color -> Picture
drawTimeline t = pictures $ Prelude.map drawEvent (Prelude.reverse $ getTimeline t)