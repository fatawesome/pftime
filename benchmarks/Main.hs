module Main where

import Naive

import AutoBench.QuickCheck
import AutoBench.Types
import Data.Default (def)

import Data.Timeline.Naive  as N
import Data.Timeline.Strict as S
import Data.Timeline.Lazy   as L

main :: IO()
main = naiveBenchmark
