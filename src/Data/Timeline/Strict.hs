{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Timeline.Strict where

import           Data.Vector

data Timeline t p = Timeline
  { timelinePlayload :: !(Vector p)
  , timelineFrom     :: !(Vector t)
  , timelineTo       :: !(Vector t)
  } deriving (Functor, Foldable, Traversable)

