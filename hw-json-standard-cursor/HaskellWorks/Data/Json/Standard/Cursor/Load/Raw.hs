module HaskellWorks.Data.Json.Standard.Cursor.Load.Raw
  ( loadRawWithIndex
  ) where

import Data.Word
import HaskellWorks.Data.FromForeignRegion
import System.IO.MMap

import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as DVS

loadRawWithIndex :: String -> IO (BS.ByteString, DVS.Vector Word64, DVS.Vector Word64)
loadRawWithIndex filename = do
  jsonFr    <- mmapFileForeignPtr filename ReadOnly Nothing
  jsonIbFr  <- mmapFileForeignPtr (filename ++ ".ib.idx") ReadOnly Nothing
  jsonBpFr  <- mmapFileForeignPtr (filename ++ ".bp.idx") ReadOnly Nothing
  let jsonBS  = fromForeignRegion jsonFr    :: BS.ByteString
  let jsonIb  = fromForeignRegion jsonIbFr  :: DVS.Vector Word64
  let jsonBp  = fromForeignRegion jsonBpFr  :: DVS.Vector Word64
  return (jsonBS, jsonIb, jsonBp)
