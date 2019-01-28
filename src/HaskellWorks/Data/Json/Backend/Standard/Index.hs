module HaskellWorks.Data.Json.Backend.Standard.Index
  ( indexJson
  ) where

import Control.Monad
import Data.Word
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Json.Backend.Standard.Cursor
import HaskellWorks.Data.Json.DecodeError
import HaskellWorks.Data.Json.Internal.Index
import HaskellWorks.Data.Json.Internal.PartialIndex
import HaskellWorks.Data.Json.PartialValue
import HaskellWorks.Data.Json.Value
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.RankSelect.Poppy512
import System.IO.MMap

import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Internal                     as BSI
import qualified Data.Vector.Storable                         as DVS
import qualified HaskellWorks.Data.ByteString                 as BS
import qualified HaskellWorks.Data.Json.Backend.Standard.Load as L
import qualified HaskellWorks.Data.Json.Backend.Standard.Slow as SLOW

indexJson :: String -> IO ()
indexJson filename = do
  JsonCursor _ ib (SimpleBalancedParens bp) _ <- L.readJson filename
  let wib = DVS.unsafeCast ib :: DVS.Vector Word8
  let wbp = DVS.unsafeCast bp :: DVS.Vector Word8
  BS.writeFile (filename ++ ".ib") (BS.toByteString wib)
  BS.writeFile (filename ++ ".bp") (BS.toByteString wbp)
