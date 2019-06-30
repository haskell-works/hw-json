{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Json.Standard.Load.Partial
  ( loadPartial
  , loadPartialWithCsPoppyIndex
  , loadPartialWithIndex
  ) where

import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Json.Internal.PartialIndex
import HaskellWorks.Data.Json.PartialValue
import HaskellWorks.Data.Json.Standard.Cursor.Generic
import HaskellWorks.Data.Json.Standard.Cursor.Load.Raw
import HaskellWorks.Data.RankSelect.CsPoppy

loadPartialWithIndex :: String -> IO JsonPartialValue
loadPartialWithIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = GenericCursor jsonBS (BitShown jsonIb) (SimpleBalancedParens jsonBp) 1
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

loadPartialWithCsPoppyIndex :: String -> IO JsonPartialValue
loadPartialWithCsPoppyIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = GenericCursor jsonBS (makeCsPoppy jsonIb) (SimpleBalancedParens jsonBp) 1
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

loadPartial :: String -> IO JsonPartialValue
loadPartial = loadPartialWithCsPoppyIndex
