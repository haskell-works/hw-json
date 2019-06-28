{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HaskellWorks.Data.Json.Backend.Simple.Fast
  ( fromByteString
  , fromForeignRegion
  , fromString
  ) where

import Foreign.ForeignPtr
import HaskellWorks.Data.Json.Backend.Simple.Cursor
import HaskellWorks.Data.RankSelect.CsPoppy

import qualified Data.ByteString                                       as BS
import qualified Data.ByteString.Char8                                 as BSC
import qualified Data.ByteString.Internal                              as BSI
import qualified HaskellWorks.Data.BalancedParens.RangeMinMax          as RMM
import qualified HaskellWorks.Data.FromForeignRegion                   as F
import qualified HaskellWorks.Data.Json.Internal.Backend.Simple.IbBp   as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Simple.ToIbBp as J

fromByteString :: BS.ByteString -> JsonCursor BS.ByteString CsPoppy (RMM.RangeMinMax CsPoppy)
fromByteString bs = JsonCursor
  { cursorText      = bs
  , interests       = makeCsPoppy ib
  , balancedParens  = RMM.mkRangeMinMax (makeCsPoppy bp)
  , cursorRank      = 1
  }
  where J.IbBp ib bp = J.toIbBp bs

fromForeignRegion :: F.ForeignRegion -> JsonCursor BS.ByteString CsPoppy (RMM.RangeMinMax CsPoppy)
fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

fromString :: String -> JsonCursor BS.ByteString CsPoppy (RMM.RangeMinMax CsPoppy)
fromString = fromByteString . BSC.pack
