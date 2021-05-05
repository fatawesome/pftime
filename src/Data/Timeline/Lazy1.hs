module Data.Timeline.Lazy1 where

import qualified Data.Timeline.Strict as Strict
  
data Chunk l p = Chunk 
  { chunkLength  :: l
  , chunkPayload :: p
  }

data Timeline t p cl 
  = Empty 
  | TimelineChunk (cl, Strict.Timeline t p) (Timeline t p cl)

