module Data.Timeline.Event where

import           Data.Timeline.Interval hiding (adjacent)
import qualified Data.Timeline.Interval as Interval

data Event t p = Event {
  interval :: Interval t,
  payload  :: p
} deriving (Show, Eq)

-- | /O(1)/.
--
-- prop> fromTuple (Interval (1,2), 'a') == Event (Interval (1,2)) 'a'
fromTuple :: (Interval t, p) -> Event t p
fromTuple (i, p) = Event i p

-- |
-- 
-- prop> mergeWith (\a b -> b) (Event (mkInterval 0 3) 'x') (Event (mkInterval 3 6) 'y') == [(Event (mkInterval 0 3) 'x'), (Event (mkInterval 3 6) 'y')]
-- prop> mergeWith (\a b -> b) (Event (mkInterval 0 3) 'x') (Event (mkInterval 1 4) 'y') == [(Event (mkInterval 0 1) 'x'), (Event (mkInterval 1 3) 'y'), (Event (mkInterval 3 4) 'y')]
-- prop> mergeWith (\a b -> b) (Event (mkInterval 0 2) 'x') (Event (mkInterval 0 1) 'y') == [(Event (mkInterval 0 1) 'y'), (Event (mkInterval 1 2) 'x')]
-- prop> mergeWith (\a b -> b) (Event (mkInterval 0 1) 'x') (Event (mkInterval 0 2) 'y') == [(Event (mkInterval 0 1) 'y'), (Event (mkInterval 1 2) 'y')]  
mergeWith 
  :: Ord t
  => (p -> p -> p) 
  -> Event t p 
  -> Event t p 
  -> [Event t p]
mergeWith f x@(Event (Interval (x1, x2)) xp) y@(Event (Interval (y1, y2)) yp)
  -- xxx
  --    yyy
  | x2 <= y1 = [x, y]

  --    xxx
  -- yyy
  | x1 >= y2 = [y, x]
  | x1 == y1 && x2 == y2 = [Event (mkInterval x1 x2) (f xp yp)]

  -- xxx
  -- yyyy
  | x1 == y1 && x2 < y2 = [ Event (mkInterval x1 x2) (f xp yp)
                          , Event (mkInterval x2 y2) yp
                          ]

  --  xxx
  -- yyyy
  | x1 > y1 && x2 == y2 = [ Event (mkInterval y1 x1) yp
                          , Event (mkInterval x1 x2) (f xp yp)
                          ]

  -- xxxx
  -- yyy
  | x1 == y1 && x2 > y2 = [ Event (mkInterval x1 y2) (f xp yp)
                          , Event (mkInterval y2 x2) xp
                          ]

  -- xxxx
  --  yyy
  | x1 < y1 && x2 == y2 = [ Event (mkInterval x1 y1) xp
                          , Event (mkInterval y1 x2) (f xp yp)
                          ]

  -- xxxxx
  --  yyy
  | x1 < y1 && x2 > y2 = [ Event (mkInterval x1 y1) xp
                         , Event (mkInterval y1 y2) (f xp yp)
                         , Event (mkInterval y2 x2) xp
                         ]
  --  xxx
  -- yyyyy 
  | x1 > y1 && x2 < y2 = [ Event (mkInterval y1 x1) yp
                         , Event (mkInterval x1 x2) (f xp yp)
                         , Event (mkInterval x2 y2) yp
                         ]
  -- xxx
  --  yyy
  | x1 < y1 && x2 < y2 = [ Event (mkInterval x1 y1) xp
                         , Event (mkInterval y1 x2) (f xp yp)
                         , Event (mkInterval x2 y2) yp
                         ]
  --  xxx
  -- yyy
  | x1 > y1 && x2 > y2 = [ Event (mkInterval y1 x1) yp
                         , Event (mkInterval x1 y2) (f xp yp)
                         , Event (mkInterval y2 x2) xp
                         ]

  -- invalid cases
  | otherwise = []


-- | /O(1)/. Events are adjancent if one strictly follows other and their payloads are equal.
--
-- >>> e1 = Event (Interval (1,2)) 'a'
-- >>> e2 = Event (Interval (2,3)) 'a'
-- >>> e1 `adjacent` e2
-- True
adjacent
  :: (Ord t, Eq p)
  => Event t p
  -> Event t p
  -> Bool
adjacent e1 e2
  = Interval.adjacent (interval e1) (interval e2) && payload e1 == payload e2
