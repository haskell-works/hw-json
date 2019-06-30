module HaskellWorks.Data.Json.Simple.Cursor.Internal.ToIbBp where

import qualified Data.ByteString                                    as BS
import qualified HaskellWorks.Data.Json.Simple.Cursor.Internal.IbBp as Z
import qualified HaskellWorks.Data.Json.Simple.Cursor.SemiIndex     as J

class ToIbBp a where
  toIbBp :: a -> Z.IbBp

instance ToIbBp BS.ByteString where
  toIbBp bs = Z.IbBp ib bp
    where J.SemiIndex _ ib bp = J.buildSemiIndex bs
