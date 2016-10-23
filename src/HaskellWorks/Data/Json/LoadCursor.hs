{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.Json.LoadCursor
  ( indexJsonCursor
  , loadByteString
  , loadJsonStrict
  , loadJsonWithCsPoppyIndex
  , loadJsonWithIndex
  , loadJsonWithPoppy512Index
  , loadJsonWithPoppy512Index2
  , loadJsonWithPoppy512SIndex
  , loadJsonWithPoppy512SIndex2
  , loadJsonWithPoppy512SMinMaxIndex
  , loadJsonWithPoppy512SMinMax2Index
  ) where

import           Control.Monad
import qualified Data.ByteString                                          as BS
import qualified Data.ByteString.Internal                                 as BSI
import qualified Data.Vector.Storable                                     as DVS
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           HaskellWorks.Data.BalancedParens.RangeMinMax
import           HaskellWorks.Data.BalancedParens.RangeMinMax2
import           HaskellWorks.Data.BalancedParens.Simple
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Decode
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Json.Succinct.Cursor
import           HaskellWorks.Data.Json.Succinct.Index
import           HaskellWorks.Data.Json.Value
import           HaskellWorks.Data.RankSelect.CsPoppy
import           HaskellWorks.Data.RankSelect.Poppy512
import           HaskellWorks.Data.RankSelect.Poppy512S
import           System.IO
import           System.IO.MMap

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
  bs <- BS.readFile path
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

loadJsonWithIndex :: String -> IO (JsonCursor BSI.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
loadJsonWithIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS (BitShown jsonIb) (SimpleBalancedParens jsonBp) 1
  return cursor

loadJsonWithPoppy512Index :: String -> IO (JsonCursor BSI.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64)))
loadJsonWithPoppy512Index filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS (makePoppy512 jsonIb) (SimpleBalancedParens jsonBp) 1
  return cursor

loadJsonWithPoppy512SIndex :: String -> IO (JsonCursor BSI.ByteString Poppy512S (SimpleBalancedParens (DVS.Vector Word64)))
loadJsonWithPoppy512SIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS (makePoppy512S jsonIb) (SimpleBalancedParens jsonBp) 1
  return cursor

loadJsonWithPoppy512SMinMaxIndex :: String -> IO (JsonCursor BSI.ByteString Poppy512S (RangeMinMax Poppy512S))
loadJsonWithPoppy512SMinMaxIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let !rangeMinMax = mkRangeMinMax (makePoppy512S jsonBp)
  let cursorL3 = JsonCursor jsonBS (makePoppy512S jsonIb) rangeMinMax 1
  return cursorL3

loadJsonWithPoppy512SMinMax2Index :: String -> IO (JsonCursor BSI.ByteString Poppy512S (RangeMinMax2 Poppy512S))
loadJsonWithPoppy512SMinMax2Index filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let !rangeMinMax2 = mkRangeMinMax2 (makePoppy512S jsonBp)
  let cursorMinMaxL2 = JsonCursor jsonBS (makePoppy512S jsonIb) rangeMinMax2 1
  return cursorMinMaxL2

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

loadJsonWithPoppy512SIndex2 :: String -> IO (JsonCursor BSI.ByteString Poppy512S (SimpleBalancedParens Poppy512S))
loadJsonWithPoppy512SIndex2 filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS (makePoppy512S jsonIb) (SimpleBalancedParens (makePoppy512S jsonBp)) 1
                :: JsonCursor BSI.ByteString Poppy512S (SimpleBalancedParens Poppy512S)
  return cursor

indexJsonCursor :: String -> IO ()
indexJsonCursor filename = do
  JsonCursor _ (BitShown ib) (SimpleBalancedParens bp) _ <- readJson filename
  let wib = DVS.unsafeCast ib :: DVS.Vector Word8
  let wbp = DVS.unsafeCast bp :: DVS.Vector Word8
  writeVector (filename ++ ".ib") wib
  writeVector (filename ++ ".bp") wbp
