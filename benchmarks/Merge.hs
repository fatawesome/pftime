module Merge where
  
import Test.QuickCheck
import Criterion.Main
  
import Data.Timeline.Naive as N
import Data.Timeline.Strict as S
import Data.Timeline.Lazy as L

naiveOfSize :: Int -> IO (N.Timeline Int Char)
naiveOfSize n = generate (resize n arbitrary :: Gen (N.Timeline Int Char))

benchOfSize :: Int -> IO Benchmark
benchOfSize n = do
  a <- naiveOfSize n
  b <- naiveOfSize n
  return $ bench (show n) $ nf (N.merge a) b 

group = bgroup "Naive.merge" <$> mapM benchOfSize [100, 200..1000]



  


