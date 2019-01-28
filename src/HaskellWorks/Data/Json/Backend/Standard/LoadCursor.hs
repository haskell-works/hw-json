{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Json.Backend.Standard.LoadCursor
  ( readJson
  , loadJsonWithCsPoppyIndex
  , loadJsonWithIndex
  , loadJsonWithPoppy512Index
  , loadJsonWithPoppy512Index2
  ) where

import Data.Word
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Json.Backend.Standard.Cursor
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.RankSelect.Poppy512
import System.IO.MMap

import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Internal                     as BSI
import qualified Data.Vector.Storable                         as DVS
import qualified HaskellWorks.Data.Json.Backend.Standard.Slow as SLOW

readJson :: String -> IO (JsonCursor BS.ByteString (DVS.Vector Word64) (SimpleBalancedParens (DVS.Vector Word64)))
readJson path = do
  bs <- readFile path
  let !cursor = SLOW.makeCursor bs
  return cursor

loadJsonRawWithIndex :: String -> IO (BS.ByteString, DVS.Vector Word64, DVS.Vector Word64)
loadJsonRawWithIndex filename = do
  jsonFr    <- mmapFileForeignPtr filename ReadOnly Nothing
  jsonIbFr  <- mmapFileForeignPtr (filename ++ ".ib") ReadOnly Nothing
  jsonBpFr  <- mmapFileForeignPtr (filename ++ ".bp") ReadOnly Nothing
  let jsonBS  = fromForeignRegion jsonFr    :: BS.ByteString
  let jsonIb  = fromForeignRegion jsonIbFr  :: DVS.Vector Word64
  let jsonBp  = fromForeignRegion jsonBpFr  :: DVS.Vector Word64
  return (jsonBS, jsonIb, jsonBp)

loadJsonWithIndex :: String -> IO (JsonCursor BSI.ByteString (DVS.Vector Word64) (SimpleBalancedParens (DVS.Vector Word64)))
loadJsonWithIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS jsonIb (SimpleBalancedParens jsonBp) 1
  return cursor

loadJsonWithPoppy512Index :: String -> IO (JsonCursor BSI.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64)))
loadJsonWithPoppy512Index filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS (makePoppy512 jsonIb) (SimpleBalancedParens jsonBp) 1
  return cursor

loadJsonWithCsPoppyIndex :: String -> IO (JsonCursor BSI.ByteString CsPoppy (SimpleBalancedParens (DVS.Vector Word64)))
loadJsonWithCsPoppyIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS (makeCsPoppy jsonIb) (SimpleBalancedParens jsonBp) 1
  return cursor

loadJsonWithPoppy512Index2 :: String -> IO (JsonCursor BSI.ByteString Poppy512 (SimpleBalancedParens Poppy512))
loadJsonWithPoppy512Index2 filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS (makePoppy512 jsonIb) (SimpleBalancedParens (makePoppy512 jsonBp)) 1
                :: JsonCursor BSI.ByteString Poppy512 (SimpleBalancedParens Poppy512)
  return cursor
