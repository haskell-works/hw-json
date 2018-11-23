{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Internal.Index where

import Control.Arrow
import Control.Monad
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Drop
import HaskellWorks.Data.Json.DecodeError
import HaskellWorks.Data.Json.Internal.CharLike
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.TreeCursor
import HaskellWorks.Data.Uncons
import Prelude                                   hiding (drop)

import qualified Data.ByteString                  as BS
import qualified Data.List                        as L
import qualified HaskellWorks.Data.BalancedParens as BP

data JsonIndex
  = JsonIndexString BS.ByteString
  | JsonIndexNumber BS.ByteString
  | JsonIndexObject [(BS.ByteString, JsonIndex)]
  | JsonIndexArray [JsonIndex]
  | JsonIndexBool Bool
  | JsonIndexNull
  deriving (Eq, Show)

class JsonIndexAt a where
  jsonIndexAt :: a -> Either DecodeError JsonIndex
