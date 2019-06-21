{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Json.Backend.Standard.Cursor.Fast
  ( Cursor
  ) where

import Data.Word
import HaskellWorks.Data.Json.Backend.Standard.Cursor.Generic
import HaskellWorks.Data.Json.Backend.Standard.Cursor.Specific
import HaskellWorks.Data.RankSelect.Poppy512

import qualified Data.ByteString                  as BS
import qualified Data.Vector.Storable             as DVS
import qualified HaskellWorks.Data.BalancedParens as BP

data Fast

instance SpecificCursor Fast where
  type CursorOf Fast = Cursor

type Cursor = GenericCursor BS.ByteString Poppy512 (BP.SimpleBalancedParens (DVS.Vector Word64))
