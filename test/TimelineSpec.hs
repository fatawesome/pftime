{-# LANGUAGE TemplateHaskell #-}

module TimelineSpec where

import Timeline
import Interval

import Test.Hspec
import Test.QuickCheck

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
      