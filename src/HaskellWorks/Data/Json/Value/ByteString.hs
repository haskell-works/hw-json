{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Value.ByteString where

import Data.ByteString
import HaskellWorks.Data.Json.Value.Internal

type JsonValue = GenJsonValue ByteString ByteString
