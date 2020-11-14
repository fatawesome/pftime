module Event where

import Interval

data Event t p = Event {
  interval :: Interval t,
  payload :: p
} deriving (Show, Eq)

adjacent
  :: (Ord t, Eq p) 
  => Event t p
  -> Event t p
  -> Bool
adjacent e1 e2 
  = Interval.adjacent (interval e1) (interval e2) && payload e1 == payload e2