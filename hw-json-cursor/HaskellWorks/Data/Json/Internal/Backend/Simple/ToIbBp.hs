module HaskellWorks.Data.Json.Internal.Backend.Simple.ToIbBp where

import qualified Data.ByteString                                     as BS
import qualified HaskellWorks.Data.Json.Backend.Simple.SemiIndex     as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Simple.IbBp as Z

class ToIbBp a where
  toIbBp :: a -> Z.IbBp

instance ToIbBp BS.ByteString where
  toIbBp bs = Z.IbBp ib bp
    where J.SemiIndex _ ib bp = J.buildSemiIndex bs
