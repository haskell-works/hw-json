{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Types
  ( CountOptions(..)
  , CreateIndexOptions(..)
  , DemoOptions(..)
  ) where

import Data.Text    (Text)
import GHC.Generics

data CreateIndexOptions = CreateIndexOptions
  { filePath     :: FilePath
  , backend      :: String
  , method       :: String
  , outputIbFile :: Maybe FilePath
  , outputBpFile :: Maybe FilePath
  } deriving (Eq, Show, Generic)

data DemoOptions = DemoOptions
  { filePath :: FilePath
  , method   :: FilePath
  } deriving (Eq, Show, Generic)

data CountOptions = CountOptions
  { inputFile  :: FilePath
  , ibIndex    :: FilePath
  , bpIndex    :: FilePath
  , expression :: Text
  } deriving (Eq, Show, Generic)
