{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Types
  ( CreateIndexOptions(..)
  , DemoOptions(..)
  ) where

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
