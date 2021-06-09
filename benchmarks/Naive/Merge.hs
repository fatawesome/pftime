module Naive.Merge where
  
import Data.Default (def)
import AutoBench.QuickCheck
import AutoBench.Types
  
import Data.Timeline.Naive
import qualified Data.Timeline.Pictoral as Pic

testData :: BinaryTestData (Timeline t p) (Timeline t p)
testData = 
  [ ( 0
    , 0
    , return empty
    , return empty 
    )
  ]

