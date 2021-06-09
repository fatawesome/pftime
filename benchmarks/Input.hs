module Input where
  
import Data.Default (def)
import AutoBench.QuickCheck
import AutoBench.Types
  
import qualified Data.Timeline.Naive as N
import qualified Data.Timeline.Strict as S
import qualified Data.Timeline.Lazy as L
import qualified Data.Timeline.Pictoral as P

a1 = P.mkPictoralTimeline "xxx xx x xxxxxx xxxxx xxxx xxxx xxxx xxxxxxx xxx xxx x xxxxx yyyy aaaa vbbbb sssss aaaa eeee tttt ttt dfasdfasdffdf sdfsafasdfa wwww xxx xx x xxxxxx xxxxx xxxx xxxx xxxx xxxxxxx xxx xxx x xxxxx yyyy aaaa vbbbb sssss aaaa eeee tttt ttt dfasdfasdffdf sdfsafasdfa wwww xxx xx x xxxxxx xxxxx xxxx xxxx xxxx xxxxxxx xxx xxx x xxxxx yyyy aaaa vbbbb sssss aaaa eeee tttt ttt dfasdfasdffdf sdfsafasdfa wwww xxx xx x xxxxxx xxxxx xxxx xxxx xxxx xxxxxxx xxx xxx x xxxxx yyyy aaaa vbbbb sssss aaaa eeee tttt ttt dfasdfasdffdf sdfsafasdfa wwww xxx xx x xxxxxx xxxxx xxxx xxxx xxxx xxxxxxx xxx xxx x xxxxx yyyy aaaa vbbbb sssss aaaa eeee tttt ttt dfasdfasdffdf sdfsafasdfa wwww xxx xx x xxxxxx xxxxx xxxx xxxx xxxx xxxxxxx xxx xxx x xxxxx yyyy aaaa vbbbb sssss aaaa eeee tttt ttt dfasdfasdffdf sdfsafasdfa wwww xxx xx x xxxxxx xxxxx xxxx xxxx xxxx xxxxxxx xxx xxx x xxxxx yyyy aaaa vbbbb sssss aaaa eeee tttt ttt dfasdfasdffdf sdfsafasdfa wwww xxx xx x xxxxxx xxxxx xxxx xxxx xxxx xxxxxxx xxx xxx x xxxxx yyyy aaaa vbbbb sssss aaaa eeee tttt ttt dfasdfasdffdf sdfsafasdfa wwww xxx xx x xxxxxx xxxxx xxxx xxxx xxxx xxxxxxx xxx xxx x xxxxx yyyy aaaa vbbbb sssss aaaa eeee tttt ttt dfasdfasdffdf sdfsafasdfa wwww xxx xx x xxxxxx xxxxx xxxx xxxx xxxx xxxxxxx xxx xxx x xxxxx yyyy aaaa vbbbb sssss aaaa eeee tttt ttt dfasdfasdffdf sdfsafasdfa wwww"
a2 = P.mkPictoralTimeline "                                  aaaaaa aaaaaaaaaaaaaaaaaa     aaaaaa aa a aaaaaa aaaaaaaaaaaa aaaaaaaaa aabbc                                   aaaaaa aaaaaaaaaaaaaaaaaa     aaaaaa aa a aaaaaa aaaaaaaaaaaa aaaaaaaaa aabbc                                  aaaaaa aaaaaaaaaaaaaaaaaa     aaaaaa aa a aaaaaa aaaaaaaaaaaa aaaaaaaaa aabbc                                  aaaaaa aaaaaaaaaaaaaaaaaa     aaaaaa aa a aaaaaa aaaaaaaaaaaa aaaaaaaaa aabbc                                  aaaaaa aaaaaaaaaaaaaaaaaa     aaaaaa aa a aaaaaa aaaaaaaaaaaa aaaaaaaaa aabbc                                  aaaaaa aaaaaaaaaaaaaaaaaa     aaaaaa aa a aaaaaa aaaaaaaaaaaa aaaaaaaaa aabbc                                  aaaaaa aaaaaaaaaaaaaaaaaa     aaaaaa aa a aaaaaa aaaaaaaaaaaa aaaaaaaaa aabbc                                  aaaaaa aaaaaaaaaaaaaaaaaa     aaaaaa aa a aaaaaa aaaaaaaaaaaa aaaaaaaaa aabbc                                  aaaaaa aaaaaaaaaaaaaaaaaa     aaaaaa aa a aaaaaa aaaaaaaaaaaa aaaaaaaaa aabbc                                  aaaaaa aaaaaaaaaaaaaaaaaa     aaaaaa aa a aaaaaa aaaaaaaaaaaa aaaaaaaaa aabbc                                  aaaaaa aaaaaaaaaaaaaaaaaa     aaaaaa aa a aaaaaa aaaaaaaaaaaa aaaaaaaaa aabbc                                  aaaaaa aaaaaaaaaaaaaaaaaa     aaaaaa aa a aaaaaa aaaaaaaaaaaa aaaaaaaaa aabbc"
a3 = P.mkPictoralTimeline "a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a"
a4 = P.mkPictoralTimeline "aaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaab"

b1 = P.mkPictoralTimeline "xxxxxxx xxxxxxx xxxxxx xxxxx" 

naiveMerge :: P.PictoralTimeline -> P.PictoralTimeline
naiveMerge b = S.toNaive $ S.fromNaive $ N.merge a b

strictMerge :: P.PictoralTimeline -> P.PictoralTimeline
strictMerge b = S.toNaive $ S.merge (S.fromNaive a) (S.fromNaive b)

lazyMerge :: P.PictoralTimeline -> P.PictoralTimeline
lazyMerge b = L.toNaive $ L.merge (L.fromNaive a) (L.fromNaive b)

--naiveMerge :: N.Timeline t p -> N.Timeline t p -> N.Timeline t p
--naiveMerge = N.merge
--
--strictMerge :: S.Timeline t p -> S.Timeline t p -> S.Timeline t p
--strictMerge = S.merge
--
--lazyMerge :: L.Timeline t p -> L.Timeline t p -> L.Timeline t p
--lazyMerge = L.merge 

ts :: TestSuite 
ts = def { _progs = ["naiveMerge", "strictMerge", "lazyMerge"]
         , _dataOpts = Gen 0 10 200
         }



  



