module Main where

import Criterion.Main
import Data.Timeline.Naive
import Data.Timeline.Pictoral

t1 = mkPictoralTimeline "xxx yyy zzz"
t2 = mkPictoralTimeline "  aaa bbb ccc"

main = defaultMain [
       bgroup "Naive.merge" [ bench "lol" $ whnf (merge t1) t2 ]
                   ]

