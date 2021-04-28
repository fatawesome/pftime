import Data.List
import Criterion.Main
import Data.Timeline.Naive
import Data.Timeline.Event
import Data.Timeline.Interval
import Data.Timeline.Overlapping

eventCreator :: Ord t => t -> t -> p -> Event t p
eventCreator t t' payload = Event (mkInterval t t') (payload) 

eventCreatorN'withoutOverlapping :: Int -> [Event Int [Char]]
eventCreatorN'withoutOverlapping n | n >= 0 = [eventCreator (t) (t+1) ("SAMPLE_TEXT") | t <- [0 .. n-1] ]
								   | otherwise = []

eventCreatorN'withOverlapping :: Int -> [Event Int [Char]]
eventCreatorN'withOverlapping n | n >= 0 = [eventCreator (t `mod` 97) ( (2*t) `mod` 113) ("SAMPLE_TEXT") | t <- [0 .. n-1] ]
				                | otherwise = []				

benchmark1 = eventCreatorN'withoutOverlapping 40
benchmark2 = eventCreatorN'withoutOverlapping 160
benchmark3 = eventCreatorN'withoutOverlapping 640
benchmark4 = eventCreatorN'withoutOverlapping 1000

benchmark1' = eventCreatorN'withOverlapping 40
benchmark2' = eventCreatorN'withOverlapping 160
benchmark3' = eventCreatorN'withOverlapping 640
benchmark4' = eventCreatorN'withOverlapping 1000

testingFunction lst = fromOverlappingTimeline (++) lst
testingFunction2 lst = fromListWith (++) lst
testingFunction3 lst = fromListWith (++) lst
testingFunction3' lst = fromListWith (\\) lst

main = defaultMain [
    bgroup "fromOverlappingTimeline/dataWithoutOverlapping" [ 
  		bench "40"   $ whnf testingFunction (OverlappingTimeline benchmark1),
  		bench "160"  $ whnf testingFunction (OverlappingTimeline benchmark2),
  		bench "640"  $ whnf testingFunction (OverlappingTimeline benchmark3),
  		bench "1000" $ whnf testingFunction (OverlappingTimeline benchmark4)
               ],
    bgroup "fromOverlappingTimeline/dataWithOverlapping" [ 
  		bench "40"   $ whnf testingFunction (OverlappingTimeline benchmark1'),
  		bench "160"  $ whnf testingFunction (OverlappingTimeline benchmark2'),
  		bench "640"  $ whnf testingFunction (OverlappingTimeline benchmark3'),
  		bench "1000" $ whnf testingFunction (OverlappingTimeline benchmark4')
               ],
    bgroup "fromListWith/dataWithoutOverlapping" [ 
  		bench "40"   $ whnf testingFunction2 benchmark1,
  		bench "160"  $ whnf testingFunction2 benchmark2,
  		bench "640"  $ whnf testingFunction2 benchmark3,
  		bench "1000" $ whnf testingFunction2 benchmark4
               ],
    bgroup "fromListWith/dataWithOverlapping" [ 
  		bench "40"   $ whnf testingFunction2 benchmark1',
  		bench "160"  $ whnf testingFunction2 benchmark2',
  		bench "640"  $ whnf testingFunction2 benchmark3',
  		bench "1000" $ whnf testingFunction2 benchmark4'
               ],
    bgroup "fromListWith/dataWithOverlapping/1000" [
    	bench "(++)" $ whnf testingFunction3 benchmark4',
    	bench "(\\)" $ whnf testingFunction3' benchmark4'
    		   ]                                            
  ]
