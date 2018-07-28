module HaskellWorks.Data.Json.Internal.IbBp where

import Data.Word

import qualified Data.ByteString                                    as BS
import qualified Data.Vector.Storable                               as DVS
import qualified HaskellWorks.Data.Json.Internal.BlankedJson        as J
import qualified HaskellWorks.Data.Json.Internal.ToBalancedParens64 as J
import qualified HaskellWorks.Data.Json.Internal.ToInterestBits64   as J

data IbBp = IbBp
  { ib :: DVS.Vector Word64
  , bp :: DVS.Vector Word64
  }

class ToIbBp a where
  toIbBp :: a -> IbBp

instance ToIbBp BS.ByteString where
  toIbBp bs = IbBp
    { ib = J.toInterestBits64 blankedJson
    , bp = J.toBalancedParens64 blankedJson
    }
    where blankedJson = J.toBlankedJsonTyped bs
