module Event where

import           Interval

data Event t p = Event {
  interval :: Interval t,
  payload  :: p
} deriving (Show, Eq)

-- | /O(1)/.
--
-- prop> fromTuple (Interval (1,2), 'a') == Event (Interval (1,2)) 'a'
fromTuple :: (Interval t, p) -> Event t p
fromTuple (i, p) = Event i p

-- | /O(1)/. Events are adjancent if one strictly follows other and their payloads are equal.
--
-- >>> e1 = Event (Interval (1,2)) 'a'
-- >>> e2 = Event (Interval (2,3)) 'a'
-- >>> e1 `adjancent` e2
-- True
adjacent
  :: (Ord t, Eq p)
  => Event t p
  -> Event t p
  -> Bool
adjacent e1 e2
  = Interval.adjacent (interval e1) (interval e2) && payload e1 == payload e2
