{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HaskellWorks.Data.Json.Backend.Simple.Fast
  ( fromByteString
  , fromForeignRegion
  , fromString
  ) where

import Data.Word
import Foreign.ForeignPtr
import HaskellWorks.Data.Json.Backend.Simple.Cursor
import HaskellWorks.Data.RankSelect.Poppy512

import qualified Data.ByteString                                       as BS
import qualified Data.ByteString.Char8                                 as BSC
import qualified Data.ByteString.Internal                              as BSI
import qualified Data.Vector.Storable                                  as DVS
import qualified HaskellWorks.Data.BalancedParens                      as BP
import qualified HaskellWorks.Data.FromForeignRegion                   as F
import qualified HaskellWorks.Data.Json.Internal.Backend.Simple.IbBp   as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Simple.ToIbBp as J

fromByteString :: BS.ByteString -> JsonCursor BS.ByteString Poppy512 (BP.SimpleBalancedParens (DVS.Vector Word64))
fromByteString bs = JsonCursor
  { cursorText      = bs
  , interests       = makePoppy512 ib
  , balancedParens  = BP.SimpleBalancedParens bp
  , cursorRank      = 1
  }
  where J.IbBp ib bp = J.toIbBp bs

fromForeignRegion :: F.ForeignRegion -> JsonCursor BS.ByteString Poppy512 (BP.SimpleBalancedParens (DVS.Vector Word64))
fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

fromString :: String -> JsonCursor BS.ByteString Poppy512 (BP.SimpleBalancedParens (DVS.Vector Word64))
fromString = fromByteString . BSC.pack
