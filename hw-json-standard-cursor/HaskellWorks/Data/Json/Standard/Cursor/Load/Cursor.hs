{-# LANGUAGE BangPatterns #-}

module HaskellWorks.Data.Json.Standard.Cursor.Load.Cursor
  ( loadCursor
  , loadCursorWithIndex
  , loadCursorWithCsPoppyIndex
  , loadCursorWithCsPoppyIndex2
  ) where

import Data.Word
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Json.Standard.Cursor.Generic
import HaskellWorks.Data.Json.Standard.Cursor.Load.Raw
import HaskellWorks.Data.RankSelect.CsPoppy

import qualified Data.ByteString.Internal                    as BSI
import qualified Data.Vector.Storable                        as DVS
import qualified HaskellWorks.Data.ByteString                as BS
import qualified HaskellWorks.Data.Json.Standard.Cursor.Fast as FAST

loadCursor :: String -> IO FAST.Cursor
loadCursor path = do
  bs <- BS.mmap path
  let !cursor = FAST.fromByteString bs
  return cursor

loadCursorWithIndex :: String -> IO (GenericCursor BSI.ByteString (DVS.Vector Word64) (SimpleBalancedParens (DVS.Vector Word64)))
loadCursorWithIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = GenericCursor jsonBS jsonIb (SimpleBalancedParens jsonBp) 1
  return cursor

loadCursorWithCsPoppyIndex :: String -> IO (GenericCursor BSI.ByteString CsPoppy (SimpleBalancedParens (DVS.Vector Word64)))
loadCursorWithCsPoppyIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = GenericCursor jsonBS (makeCsPoppy jsonIb) (SimpleBalancedParens jsonBp) 1
  return cursor

loadCursorWithCsPoppyIndex2 :: String -> IO (GenericCursor BSI.ByteString CsPoppy (SimpleBalancedParens CsPoppy))
loadCursorWithCsPoppyIndex2 filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = GenericCursor jsonBS (makeCsPoppy jsonIb) (SimpleBalancedParens (makeCsPoppy jsonBp)) 1
                :: GenericCursor BSI.ByteString CsPoppy (SimpleBalancedParens CsPoppy)
  return cursor
