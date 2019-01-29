{-# LANGUAGE BangPatterns #-}

module HaskellWorks.Data.Json.Backend.Standard.Load.Cursor
  ( loadCursor
  ) where

import Data.Word
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Json.Backend.Standard.Cursor
import HaskellWorks.Data.Json.Backend.Standard.Load.Raw
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.RankSelect.Poppy512

import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Internal                     as BSI
import qualified Data.Vector.Storable                         as DVS
import qualified HaskellWorks.Data.ByteString                 as BS
import qualified HaskellWorks.Data.Json.Backend.Standard.Slow as SLOW

loadCursor :: String -> IO (JsonCursor BS.ByteString (DVS.Vector Word64) (SimpleBalancedParens (DVS.Vector Word64)))
loadCursor path = do
  bs <- BS.mmap path
  let !cursor = SLOW.makeCursor bs
  return cursor

loadCursorWithIndex :: String -> IO (JsonCursor BSI.ByteString (DVS.Vector Word64) (SimpleBalancedParens (DVS.Vector Word64)))
loadCursorWithIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = JsonCursor jsonBS jsonIb (SimpleBalancedParens jsonBp) 1
  return cursor

loadCursorWithPoppy512Index :: String -> IO (JsonCursor BSI.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64)))
loadCursorWithPoppy512Index filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = JsonCursor jsonBS (makePoppy512 jsonIb) (SimpleBalancedParens jsonBp) 1
  return cursor

loadCursorWithCsPoppyIndex :: String -> IO (JsonCursor BSI.ByteString CsPoppy (SimpleBalancedParens (DVS.Vector Word64)))
loadCursorWithCsPoppyIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = JsonCursor jsonBS (makeCsPoppy jsonIb) (SimpleBalancedParens jsonBp) 1
  return cursor

loadCursorWithPoppy512Index2 :: String -> IO (JsonCursor BSI.ByteString Poppy512 (SimpleBalancedParens Poppy512))
loadCursorWithPoppy512Index2 filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = JsonCursor jsonBS (makePoppy512 jsonIb) (SimpleBalancedParens (makePoppy512 jsonBp)) 1
                :: JsonCursor BSI.ByteString Poppy512 (SimpleBalancedParens Poppy512)
  return cursor
