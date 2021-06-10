{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.Timeline.Combined where
  
import           GHC.Generics
import           Test.QuickCheck
import           Control.DeepSeq

import qualified Data.Timeline.Naive as N
import qualified Data.Timeline.Strict as S
import qualified Data.Timeline.Lazy as L
  
data Combined t p = Combined
  { naive :: N.Timeline t p
  , strict :: S.Timeline t p
  , lazy ::  L.Timeline t p
  } deriving (Generic, NFData)

data OneOf t p
  = OneOfNaive (N.Timeline t p)
  | OneOfStrict (S.Timeline t p)
  | OneOfLazy (L.Timeline t p)
  deriving (Generic, NFData)

instance (Ord t, Arbitrary t, Arbitrary p) => Arbitrary (Combined t p) where
  arbitrary = do
    naive <- arbitrary
    let strict = S.fromNaive naive
        lazy = L.fromNaive naive
    return Combined { naive = naive, strict = strict, lazy = lazy }
