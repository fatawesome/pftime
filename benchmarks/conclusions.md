# Conclusions
1) function fromOverlappingTimeline works in the same time on data with and without overlapping

2) function fromListWith. time grows faster on data with overlapping than on data without overlapping

3) fromOverlappingTimeline works much faster than fromListwith. about 2 times faster(without overlap.) and 4 times(with overlap.)

4) if you pass the operator (\ \\) as the first function in fromListWith then time will increase slightly compared to operator (++). Most likely this is due to the fact that in order to use (\ \\) we should import Data.List, and (++) is in Prelude  
