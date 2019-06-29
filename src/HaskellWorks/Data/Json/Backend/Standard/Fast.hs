{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HaskellWorks.Data.Json.Backend.Standard.Fast
  ( fromByteString
  , fromForeignRegion
  , fromString
  ) where

import Foreign.ForeignPtr
import HaskellWorks.Data.Json.Backend.Standard.Cursor.Fast
import HaskellWorks.Data.Json.Backend.Standard.Cursor.Generic
import HaskellWorks.Data.RankSelect.CsPoppy

import qualified Data.ByteString                                       as BS
import qualified Data.ByteString.Char8                                 as BSC
import qualified Data.ByteString.Internal                              as BSI
import qualified HaskellWorks.Data.BalancedParens.RangeMin             as RM
import qualified HaskellWorks.Data.FromForeignRegion                   as F
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.IbBp as J

fromByteString :: BS.ByteString -> Cursor
fromByteString bs = GenericCursor
  { cursorText      = bs
  , interests       = makeCsPoppy ib
  , balancedParens  = RM.mkRangeMin bp
  , cursorRank      = 1
  }
  where J.IbBp ib bp = J.toIbBp bs

fromForeignRegion :: F.ForeignRegion -> Cursor
fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

fromString :: String -> Cursor
fromString = fromByteString . BSC.pack
