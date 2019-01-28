{-# LANGUAGE BangPatterns #-}

module HaskellWorks.Data.Json.Backend.Standard.Load.Cursor
  ( loadCursor
  ) where

import Data.Word
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Json.Backend.Standard.Cursor

import qualified Data.ByteString                              as BS
import qualified Data.Vector.Storable                         as DVS
import qualified HaskellWorks.Data.ByteString                 as BS
import qualified HaskellWorks.Data.Json.Backend.Standard.Slow as SLOW

loadCursor :: String -> IO (JsonCursor BS.ByteString (DVS.Vector Word64) (SimpleBalancedParens (DVS.Vector Word64)))
loadCursor path = do
  bs <- BS.mmap path
  let !cursor = SLOW.makeCursor bs
  return cursor
