module HaskellWorks.Data.Json.Internal.Backend.Simple.IbBp where

import Data.Word

import qualified Data.Vector.Storable as DVS

data IbBp = IbBp
  { ib :: DVS.Vector Word64
  , bp :: DVS.Vector Word64
  }
