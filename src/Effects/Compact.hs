module Effects.Compact where

import Polysemy (Embed, Member, Sem)
import qualified Polysemy as P
import qualified GHC.Compact as C


data CompactData m a where
  CompactData :: a -> CompactData m (C.Compact a)

P.makeSem ''CompactData

runCompactData :: Member (Embed IO) r
  => Sem (CompactData : r) a
  -> Sem r a
runCompactData = P.interpret $ \case
  CompactData d -> P.embed $ C.compactWithSharing d





