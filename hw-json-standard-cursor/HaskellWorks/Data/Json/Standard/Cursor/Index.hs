{-# LANGUAGE BangPatterns #-}

module HaskellWorks.Data.Json.Standard.Cursor.Index
  ( indexJson
  ) where

import Data.Word
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Json.Standard.Cursor.Generic

import qualified Data.ByteString                             as BS
import qualified Data.Vector.Storable                        as DVS
import qualified HaskellWorks.Data.ByteString                as BS
import qualified HaskellWorks.Data.Json.Standard.Cursor.Slow as SLOW

indexJson :: String -> IO ()
indexJson filename = do
  bs <- BS.mmap filename
  -- We use the SLOW reference implementation because we are writing to a file and will never query.
  let GenericCursor _ !ib (SimpleBalancedParens !bp) _ = SLOW.fromByteString bs
  let wib = DVS.unsafeCast ib :: DVS.Vector Word8
  let wbp = DVS.unsafeCast bp :: DVS.Vector Word8
  BS.writeFile (filename ++ ".ib.idx") (BS.toByteString wib)
  BS.writeFile (filename ++ ".bp.idx") (BS.toByteString wbp)
