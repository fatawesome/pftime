module Test where

import Data.List
import Criterion.Main
import Data.Timeline.Naive
import Data.Timeline.Event
import Data.Timeline.Interval
import Data.Timeline.Overlapping 

eventCreatorN'withoutOverlapping :: Int -> [Event Int [Char]]
eventCreatorN'withoutOverlapping n 
  | n >= 0 = [eventCreator t (t+1) "SAMPLE_TEXT" | t <- [0 .. n-1] ]
  | otherwise = []

benchmark1 = eventCreatorN'withoutOverlapping 500
benchmark2 = eventCreatorN'withoutOverlapping 1000
benchmark3 = eventCreatorN'withoutOverlapping 5000
benchmark4 = eventCreatorN'withoutOverlapping 10000

benchmark1' = eventCreatorN'withOverlapping 500
benchmark2' = eventCreatorN'withOverlapping 1000
benchmark3' = eventCreatorN'withOverlapping 5000
benchmark4' = eventCreatorN'withOverlapping 10000

testingFunction lst = fromOverlappingTimeline (++) lst
testingFunction2 lst = fromListWith (++) lst
testingFunction3 lst = fromListWith (++) lst
testingFunction3' lst = fromListWith (\\) lst

testBench = defaultMain [
    bgroup "fromOverlappingTimeline/dataWithoutOverlapping" [ 
      bench "500"   $ whnf testingFunction (OverlappingTimeline benchmark1),
      bench "1000"  $ whnf testingFunction (OverlappingTimeline benchmark2),
      bench "5000"  $ whnf testingFunction (OverlappingTimeline benchmark3),
      bench "10000" $ whnf testingFunction (OverlappingTimeline benchmark4)
    ],
    bgroup "fromOverlappingTimeline/dataWithOverlapping" [ 
      bench "500"   $ whnf testingFunction (OverlappingTimeline benchmark1'),
      bench "1000"  $ whnf testingFunction (OverlappingTimeline benchmark2'),
      bench "5000"  $ whnf testingFunction (OverlappingTimeline benchmark3'),
      bench "10000" $ whnf testingFunction (OverlappingTimeline benchmark4')
    ],
    bgroup "fromListWith/dataWithoutOverlapping" [ 
      bench "500"   $ whnf testingFunction2 benchmark1,
      bench "1000"  $ whnf testingFunction2 benchmark2,
      bench "5000"  $ whnf testingFunction2 benchmark3,
      bench "10000" $ whnf testingFunction2 benchmark4
    ],
    bgroup "fromListWith/dataWithOverlapping" [ 
      bench "500"   $ whnf testingFunction2 benchmark1',
      bench "1000"  $ whnf testingFunction2 benchmark2',
      bench "5000"  $ whnf testingFunction2 benchmark3',
      bench "10000" $ whnf testingFunction2 benchmark4'
    ],
    bgroup "fromListWith/dataWithOverlapping/1000" [
      bench "(++)" $ whnf testingFunction3 benchmark4',
      bench "(\\)" $ whnf testingFunction3' benchmark4'
    ]                                            
  ]
