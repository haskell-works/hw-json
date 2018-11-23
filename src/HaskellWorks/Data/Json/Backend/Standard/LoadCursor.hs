{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Json.Backend.Standard.LoadCursor
  ( indexJsonCursor
  , loadJsonStrict
  , loadJsonWithCsPoppyIndex
  , loadJsonWithIndex
  , loadJsonWithPoppy512Index
  , loadJsonWithPoppy512Index2
  ) where

import Control.Monad
import Data.Word
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Json.Cursor
import HaskellWorks.Data.Json.DecodeError
import HaskellWorks.Data.Json.Internal.Index
import HaskellWorks.Data.Json.Value
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.RankSelect.Poppy512
import System.IO.MMap

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Internal     as BSI
import qualified Data.Vector.Storable         as DVS
import qualified HaskellWorks.Data.ByteString as BS

readJson :: String -> IO (JsonCursor BS.ByteString (DVS.Vector Word64) (SimpleBalancedParens (DVS.Vector Word64)))
readJson path = do
  bs <- BS.readFile path
  let !cursor = fromByteString bs :: JsonCursor BS.ByteString (DVS.Vector Word64) (SimpleBalancedParens (DVS.Vector Word64))
  return cursor

loadJsonStrict :: String -> IO (Either DecodeError [JsonValue])
loadJsonStrict filename = do
  !cursor <- readJson filename
  let !jsonResult = (jsonIndexAt >=> jsonValueAt) cursor
  return $ (:[]) `fmap` jsonResult

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

indexJsonCursor :: String -> IO ()
indexJsonCursor filename = do
  JsonCursor _  ib (SimpleBalancedParens bp) _ <- readJson filename
  let wib = DVS.unsafeCast ib :: DVS.Vector Word8
  let wbp = DVS.unsafeCast bp :: DVS.Vector Word8
  BS.writeFile (filename ++ ".ib") (BS.toByteString wib)
  BS.writeFile (filename ++ ".bp") (BS.toByteString wbp)
