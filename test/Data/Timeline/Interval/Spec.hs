{-# LANGUAGE TypeApplications #-}

module Data.Timeline.Interval.Spec where

--import Interval
--
import Test.Hspec
import Test.QuickCheck
--
--instance (Ord t, Arbitrary t) => Arbitrary (Interval t) where
--  arbitrary = mkInterval <$> arbitrary
--  
--isShorter :: (Num t, Ord t) => Interval t -> Interval t -> Bool
--isShorter x@(Interval (x1, x2)) y@(Interval (y1, y2)) 
--  = case x `intersectIntervals` y of
--    Just (Interval (i1, i2)) -> (i2 - i1) <= xLength && (i2 - i1) <= yLength 
--    Nothing -> True
--  where
--    xLength = x2 - x1
--    yLength = y2 - y1
--
spec :: Spec
spec = do
  describe "intersect" $ do
    it "works" $ 1 == 1