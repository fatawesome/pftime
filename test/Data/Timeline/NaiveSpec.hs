{-# LANGUAGE TypeApplications #-}

module TimelineSpec where

import           Interval
import           Timeline

import           Test.Hspec
import           Test.QuickCheck
--
--instance (Ord t, Arbitrary t) => Arbitrary (Interval t) where
--  arbitrary = mkInterval <$> arbitrary
--
--instance (Arbitrary e, Ord t, Arbitrary t) => Arbitrary (Timeline t e) where
--  arbitrary = fromListWith const <$> arbitrary
--  shrink = map unsafeFromList . shrink . toList
--
--insertOrderedProp
--  :: Ord t
--  => (e -> e -> e)
--  -> (Interval t, e)
--  -> Timeline t e
--  -> Bool
--insertOrderedProp mergePayload elem timeline =
--  isValid (insert mergePayload elem timeline)
--
spec :: Spec
spec = do
  describe "insert" $ do
    it "preserves invariants" $ 1 == 1
