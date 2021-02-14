{-# LANGUAGE TypeFamilies #-}
module Data.Timeline.Class where

import qualified Data.Timeline.Event as Event

type Event timeline = Event.Event (Timestamp timeline) (Payload timeline)

class Timeline timeline where
  type Timestamp timeline :: *
  type Payload timeline :: *

  insert :: Event timeline -> timeline -> timeline

