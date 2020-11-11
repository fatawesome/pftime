module Event where

import Interval

newtype Event t p = Event {getEvent :: (Interval t, p)}