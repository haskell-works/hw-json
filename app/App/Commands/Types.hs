{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Types
  ( CountAesonOptions(..)
  , CountOptions(..)
  , CreateIndexOptions(..)
  , DemoOptions(..)
  , FileIndexes(..)
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
  , indexes    :: Maybe FileIndexes
  , expression :: Text
  } deriving (Eq, Show, Generic)

data CountAesonOptions = CountAesonOptions
  { inputFile  :: FilePath
  , expression :: Text
  } deriving (Eq, Show, Generic)

data FileIndexes = FileIndexes
  { ibIndex :: FilePath
  , bpIndex :: FilePath
  } deriving (Eq, Show, Generic)
