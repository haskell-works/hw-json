module HaskellWorks.Data.Json.Simple.Cursor.Internal.IbBp where

import Data.Word

import qualified Data.Vector.Storable as DVS

data IbBp = IbBp
  { ib :: DVS.Vector Word64
  , bp :: DVS.Vector Word64
  }
