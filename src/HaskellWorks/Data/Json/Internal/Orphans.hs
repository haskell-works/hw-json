{-# OPTIONS_GHC -fno-warn-orphans #-}

module HaskellWorks.Data.Json.Internal.Orphans where

import Data.Text
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Data.Text as T

instance Pretty Text where
  pretty s = pretty (T.unpack s)
