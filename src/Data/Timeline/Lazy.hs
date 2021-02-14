module Data.Timeline.Lazy where

import qualified Data.Timeline.Strict as Strict
import           Data.Vector

data Timeline t p
  = Empty
  | Chunk !(Strict.Timeline t p) (Timeline t p)


