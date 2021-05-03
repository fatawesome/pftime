module Naive where

import System.Random
import Criterion.Main
import Data.Timeline.Naive
import Data.Timeline.Event
import Helpers

-- insertion stuff
timeline10 = Timeline   $ eventCreatorN'withoutOverlapping 10
timeline50 = Timeline   $ eventCreatorN'withoutOverlapping 50
timeline100 = Timeline  $ eventCreatorN'withoutOverlapping 100
timeline250 = Timeline  $ eventCreatorN'withoutOverlapping 250
timeline500 = Timeline  $ eventCreatorN'withoutOverlapping 500
timeline1000 = Timeline $ eventCreatorN'withoutOverlapping 1000
timeline1500 = Timeline $ eventCreatorN'withoutOverlapping 1500
eventInsertion = eventCreator 1250000 14000000 "test"
fn = insert (++) eventInsertion
insertGroup = bgroup "insert" [
                    bench "10"   $ whnf fn timeline10,
                    bench "50"   $ whnf fn timeline50,
                    bench "100"  $ whnf fn timeline100,
                    bench "250"  $ whnf fn timeline250,
                    bench "500"  $ whnf fn timeline500,
                    bench "1000" $ whnf fn timeline1000,
                    bench "1500" $ whnf fn timeline1500
              ]    
              
-- merge stuff
timelineA = Timeline $ eventCreatorN'withoutOverlapping 100
timelineB = Timeline $ eventCreatorN'withoutOverlapping 200
timelineC = Timeline $ eventCreatorN'withoutOverlapping 10000
timelineD = Timeline $ eventCreatorN'withoutOverlapping 20000
mergeGroup = bgroup "merge" [ bench "A+B" $ whnf (merge timelineA) timelineB
                            , bench "C+D" $ whnf (merge timelineC) timelineD
                            ]

naiveBenchmark = defaultMain [ insertGroup, mergeGroup ]
                   