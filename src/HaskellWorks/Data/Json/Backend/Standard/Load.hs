{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Json.Backend.Standard.Load
  ( loadCursor
  , loadJson
  , loadJsonPartial
  , loadJsonWithCsPoppyIndex
  , loadJsonWithIndex
  , loadJsonWithPoppy512Index
  , loadJsonWithPoppy512Index2
  ) where

import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Json.Backend.Standard.Cursor
import HaskellWorks.Data.Json.Backend.Standard.Load.Cursor
import HaskellWorks.Data.Json.Backend.Standard.Load.Raw
import HaskellWorks.Data.Json.Internal.PartialIndex
import HaskellWorks.Data.Json.PartialValue
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.RankSelect.Poppy512

import qualified Data.ByteString.Internal as BSI

loadJsonWithIndex :: String -> IO JsonPartialValue
loadJsonWithIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = JsonCursor jsonBS (BitShown jsonIb) (SimpleBalancedParens jsonBp) 1
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

loadJsonWithPoppy512Index :: String -> IO JsonPartialValue
loadJsonWithPoppy512Index filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = JsonCursor jsonBS (makePoppy512 jsonIb) (SimpleBalancedParens jsonBp) 1
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

loadJsonWithCsPoppyIndex :: String -> IO JsonPartialValue
loadJsonWithCsPoppyIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = JsonCursor jsonBS (makeCsPoppy jsonIb) (SimpleBalancedParens jsonBp) 1
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

loadJsonWithPoppy512Index2 :: String -> IO JsonPartialValue
loadJsonWithPoppy512Index2 filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = JsonCursor jsonBS (makePoppy512 jsonIb) (SimpleBalancedParens (makePoppy512 jsonBp)) 1
                :: JsonCursor BSI.ByteString Poppy512 (SimpleBalancedParens Poppy512)
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

loadJson :: String -> IO JsonPartialValue
loadJson = loadJsonWithCsPoppyIndex

loadJsonPartial :: String -> IO JsonPartialValue
loadJsonPartial filename = do
  !cursor <- loadCursor filename
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult
