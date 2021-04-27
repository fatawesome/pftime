module Visuals.Interval where

import CodeWorld
import Data.Timeline.Interval

boundaryHeight :: Double
boundaryHeight = 0.4

boundary :: Point -> Picture
boundary (x, y) = polyline [from, to]
  where
    from = (x, y + boundaryHeight / 2)
    to   = (x, y - boundaryHeight / 2) 

drawInterval :: Interval Int -> Picture
drawInterval (Interval (f, t)) = polyline [from, to] <> boundary from <> boundary to 
  where
   from = (fromIntegral f, 0)
   to = (fromIntegral t, 0)
