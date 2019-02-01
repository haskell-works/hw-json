module HaskellWorks.Data.Json.Backend.Standard.Index
  ( indexJson
  ) where

import Data.Word
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Json.Backend.Standard.Cursor

import qualified Data.ByteString                                     as BS
import qualified Data.Vector.Storable                                as DVS
import qualified HaskellWorks.Data.ByteString                        as BS
import qualified HaskellWorks.Data.Json.Backend.Standard.Load.Cursor as L

indexJson :: String -> IO ()
indexJson filename = do
  JsonCursor _ ib (SimpleBalancedParens bp) _ <- L.loadCursor filename
  let wib = DVS.unsafeCast ib :: DVS.Vector Word8
  let wbp = DVS.unsafeCast bp :: DVS.Vector Word8
  BS.writeFile (filename ++ ".ib.idx") (BS.toByteString wib)
  BS.writeFile (filename ++ ".bp.idx") (BS.toByteString wbp)
