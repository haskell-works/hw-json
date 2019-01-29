{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Json.Backend.Standard.LoadCursor
  ( loadCursor
  , loadJsonWithCsPoppyIndex
  , loadJsonWithIndex
  , loadJsonWithPoppy512Index
  , loadJsonWithPoppy512Index2
  ) where

import Data.Word
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Json.Backend.Standard.Cursor
import HaskellWorks.Data.Json.Backend.Standard.Load.Cursor
import HaskellWorks.Data.Json.Backend.Standard.Load.Raw
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.RankSelect.Poppy512

import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Storable     as DVS

loadJsonWithIndex :: String -> IO (JsonCursor BSI.ByteString (DVS.Vector Word64) (SimpleBalancedParens (DVS.Vector Word64)))
loadJsonWithIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = JsonCursor jsonBS jsonIb (SimpleBalancedParens jsonBp) 1
  return cursor

loadJsonWithPoppy512Index :: String -> IO (JsonCursor BSI.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64)))
loadJsonWithPoppy512Index filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = JsonCursor jsonBS (makePoppy512 jsonIb) (SimpleBalancedParens jsonBp) 1
  return cursor

loadJsonWithCsPoppyIndex :: String -> IO (JsonCursor BSI.ByteString CsPoppy (SimpleBalancedParens (DVS.Vector Word64)))
loadJsonWithCsPoppyIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = JsonCursor jsonBS (makeCsPoppy jsonIb) (SimpleBalancedParens jsonBp) 1
  return cursor

loadJsonWithPoppy512Index2 :: String -> IO (JsonCursor BSI.ByteString Poppy512 (SimpleBalancedParens Poppy512))
loadJsonWithPoppy512Index2 filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = JsonCursor jsonBS (makePoppy512 jsonIb) (SimpleBalancedParens (makePoppy512 jsonBp)) 1
                :: JsonCursor BSI.ByteString Poppy512 (SimpleBalancedParens Poppy512)
  return cursor
