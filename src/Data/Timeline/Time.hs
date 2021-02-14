{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Timeline.Time where

newtype Relative t = Relative { getRelative :: t }
  deriving (Eq, Ord, Show, Num)

newtype Absolute t = Absolute { getAbsolute :: t }
  deriving (Eq, Ord, Show)
