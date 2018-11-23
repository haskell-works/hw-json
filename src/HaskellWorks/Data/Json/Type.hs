{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Type
  ( JsonType(..)
  , JsonTypeAt(..)
  ) where

import HaskellWorks.Data.Positioning
import Prelude                       hiding (drop)

{-# ANN module ("HLint: Reduce duplication" :: String) #-}

data JsonType
  = JsonTypeArray
  | JsonTypeBool
  | JsonTypeNull
  | JsonTypeNumber
  | JsonTypeObject
  | JsonTypeString
  deriving (Eq, Show)

class JsonTypeAt a where
  jsonTypeAtPosition :: Position -> a -> Maybe JsonType
  jsonTypeAt :: a -> Maybe JsonType
