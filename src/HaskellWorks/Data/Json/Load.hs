{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Json.Load
  ( indexJson
  , loadByteString
  , loadJson
  , loadJsonPartial
  , loadJsonStrict
  , loadJsonWithCsPoppyIndex
  , loadJsonWithIndex
  , loadJsonWithPoppy512Index
  , loadJsonWithPoppy512Index2
  ) where

import Control.Monad
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Json.Cursor
import HaskellWorks.Data.Json.DecodeError
import HaskellWorks.Data.Json.PartialValue
import HaskellWorks.Data.Json.Succinct.Index
import HaskellWorks.Data.Json.Succinct.PartialIndex
import HaskellWorks.Data.Json.Value
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.RankSelect.Poppy512
import System.IO
import System.IO.MMap

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Storable     as DVS

-- | Write out a vector verbatim into an open file handle.
hPutVector :: forall a. Storable a => Handle -> DVS.Vector a -> IO ()
hPutVector h v = withForeignPtr fp $ \p -> hPutBuf h (p `plusPtr` offset) sz
      where
        (fp, offset, n) = DVS.unsafeToForeignPtr v
        eltsize = sizeOf (undefined :: a)
        sz = n * eltsize

-- | Write the vector verbatim to a file.
writeVector :: forall a. Storable a => FilePath -> DVS.Vector a -> IO ()
writeVector fp v = withFile fp WriteMode $ \h -> hPutVector h v

readJson :: String -> IO (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
readJson path = do
  bs <- loadByteString path
  putStrLn "Read file"
  let !cursor = fromByteString bs :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
  putStrLn "Created cursor"
  return cursor

loadJsonStrict :: String -> IO (Either DecodeError [JsonValue])
loadJsonStrict filename = do
  !cursor <- readJson filename
  let !jsonResult = (jsonIndexAt >=> jsonValueAt) cursor
  return $ (:[]) `fmap` jsonResult

loadByteString :: FilePath -> IO BS.ByteString
loadByteString filepath = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr filepath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  return bs

loadJsonRawWithIndex :: String -> IO (BS.ByteString, DVS.Vector Word64, DVS.Vector Word64)
loadJsonRawWithIndex filename = do
  jsonFr    <- mmapFileForeignPtr filename ReadOnly Nothing
  jsonIbFr  <- mmapFileForeignPtr (filename ++ ".ib") ReadOnly Nothing
  jsonBpFr  <- mmapFileForeignPtr (filename ++ ".bp") ReadOnly Nothing
  let jsonBS  = fromForeignRegion jsonFr    :: BS.ByteString
  let jsonIb  = fromForeignRegion jsonIbFr  :: DVS.Vector Word64
  let jsonBp  = fromForeignRegion jsonBpFr  :: DVS.Vector Word64
  return (jsonBS, jsonIb, jsonBp)

loadJsonWithIndex :: String -> IO JsonPartialValue
loadJsonWithIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS (BitShown jsonIb) (SimpleBalancedParens jsonBp) 1
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

loadJsonWithPoppy512Index :: String -> IO JsonPartialValue
loadJsonWithPoppy512Index filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS (makePoppy512 jsonIb) (SimpleBalancedParens jsonBp) 1
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

loadJsonWithCsPoppyIndex :: String -> IO JsonPartialValue
loadJsonWithCsPoppyIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS (makeCsPoppy jsonIb) (SimpleBalancedParens jsonBp) 1
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

loadJsonWithPoppy512Index2 :: String -> IO JsonPartialValue
loadJsonWithPoppy512Index2 filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS (makePoppy512 jsonIb) (SimpleBalancedParens (makePoppy512 jsonBp)) 1
                :: JsonCursor BSI.ByteString Poppy512 (SimpleBalancedParens Poppy512)
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

indexJson :: String -> IO ()
indexJson filename = do
  JsonCursor _ (BitShown ib) (SimpleBalancedParens bp) _ <- readJson filename
  let wib = DVS.unsafeCast ib :: DVS.Vector Word8
  let wbp = DVS.unsafeCast bp :: DVS.Vector Word8
  writeVector (filename ++ ".ib") wib
  writeVector (filename ++ ".bp") wbp

loadJson :: String -> IO JsonPartialValue
loadJson = loadJsonWithCsPoppyIndex

loadJsonPartial :: String -> IO JsonPartialValue
loadJsonPartial filename = do
  !cursor <- readJson filename
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult
