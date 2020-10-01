{-# LANGUAGE TemplateHaskell #-}

module TimelineSpec where

import           Interval
import           Timeline

import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary t => Arbitrary (Interval t) where
  arbitrary = do
    left <- arbitrary
    right <- arbitrary
    return (Interval (left, right))

-- instance (Arbitrary t, Arbitrary e) => Arbitrary (Timeline t e) where
--   arbitrary = sized arbitrarySizedTimeline

-- abritrarySizedTimeline
--   :: (Ord t, Arbitrary t, Arbitrary e)
--   => Int
--   -> Gen (Timeline t e)
-- abritrarySizedTimeline n = do
--   t <- arbitrary1
--   e <- arbitrary2
--   m <- choose 0 n
--   xs = Timleine (vectorOf (Interval (t)))


ordered :: Ord t => Timeline t e -> Bool
ordered (Timeline xs) = and (zipWith (\x y -> fst x < fst y) xs (drop 1 xs))
  
insertOrderedProp
  :: Ord t
  => (e -> e -> e)
  -> (Interval t, e)
  -> Timeline t e
  -> Property
insertOrderedProp mergePayload elem timeline =
  ordered timeline ==> ordered (insert mergePayload elem timeline)

spec :: Spec
spec = do
  describe "insert" $ do
    it "should preserve ordered invariant" $ insertOrderedProp

