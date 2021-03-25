module Visuals.Helpers where

import CodeWorld
import Data.Text as Text
import Data.Timeline.Naive
import Data.Timeline.Interval
import Visuals.Event
import Visuals.Interval

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

drawInt :: Int -> Picture
drawInt = dilated 0.5 . lettering . Text.pack . show

xAxis :: Int -> Picture
xAxis len = pictures $ Prelude.map xAxisSegment intervals
  where
    intervals = Prelude.take len $ iterate (\(a, b) -> (a + 1, b + 1)) (0, 1)
    intervalFromPair (a, b) = mkInterval a b 
    xAxisSegment (a, b) 
      = translated (fromIntegral a) (boundaryHeight + 0.2) (drawInt a)
          <> drawInterval (intervalFromPair (a, b))
    intLabel :: Int -> Picture
    intLabel = dilated 0.5 . lettering . Text.pack . show
    
numbers :: Int -> Picture
numbers n = pictures $ Prelude.map 
                        (\x -> translated (fromIntegral x) 0 (drawInt x)) 
                        (Prelude.take n $ iterate (+ 1) 0) 