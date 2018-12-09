{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HaskellWorks.Data.Json.Backend.Standard.Slow
  ( makeCursor
  ) where

import Data.Word
import Foreign.ForeignPtr
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Json.Backend.Standard.Cursor

import qualified Data.ByteString                                       as BS
import qualified Data.ByteString.Char8                                 as BSC
import qualified Data.ByteString.Internal                              as BSI
import qualified Data.Vector.Storable                                  as DVS
import qualified HaskellWorks.Data.BalancedParens                      as BP
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.IbBp as J

class MakeCursor a where
  makeCursor :: a -> JsonCursor BS.ByteString (DVS.Vector Word64) (BP.SimpleBalancedParens (DVS.Vector Word64))

instance MakeCursor BS.ByteString where
  makeCursor bs = JsonCursor
    { cursorText      = bs
    , interests       = ib
    , balancedParens  = BP.SimpleBalancedParens bp
    , cursorRank      = 1
    }
    where J.IbBp ib bp = J.toIbBp bs

instance MakeCursor String where
  makeCursor = makeCursor . BSC.pack

instance MakeCursor ForeignRegion where
  makeCursor (fptr, offset, size) = makeCursor (BSI.fromForeignPtr (castForeignPtr fptr) offset size)
