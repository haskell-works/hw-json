{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Json.Backend.Standard.Cursor.Slow
  ( Cursor
  ) where

import Data.Word
import HaskellWorks.Data.Json.Backend.Standard.Cursor.Generic
import HaskellWorks.Data.Json.Backend.Standard.Cursor.Specific

import qualified Data.ByteString                  as BS
import qualified Data.Vector.Storable             as DVS
import qualified HaskellWorks.Data.BalancedParens as BP

data Slow

instance SpecificCursor Slow where
  type CursorOf Slow = Cursor

type Cursor = GenericCursor BS.ByteString (DVS.Vector Word64) (BP.SimpleBalancedParens (DVS.Vector Word64))
