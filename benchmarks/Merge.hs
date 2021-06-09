module Merge where
  
import Data.Default (def)
import AutoBench.QuickCheck
import AutoBench.Types
  
import qualified Data.Timeline.Naive as N
import qualified Data.Timeline.Strict as S
import qualified Data.Timeline.Lazy as L
import qualified Data.Timeline.Pictoral as P

naiveMerge :: P.PictoralTimeline -> P.PictoralTimeline -> P.PictoralTimeline
naiveMerge = N.merge

strictMerge :: P.PictoralTimeline -> P.PictoralTimeline -> P.PictoralTimeline
strictMerge a b = S.toNaive $ S.merge (S.fromNaive a) (S.fromNaive b)

lazyMerge :: P.PictoralTimeline -> P.PictoralTimeline -> P.PictoralTimeline
lazyMerge a b = L.toNaive $ L.merge (L.fromNaive a) (L.fromNaive b)

ts :: TestSuite 
ts  = def



  


