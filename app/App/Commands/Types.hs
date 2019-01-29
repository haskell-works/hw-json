module App.Commands.Types
  ( CreateIndexOptions(..)
  , DemoOptions(..)
  ) where

data CreateIndexOptions = CreateIndexOptions
  { _createIndexOptionsFilePath     :: FilePath
  , _createIndexOptionsBackend      :: String
  , _createIndexOptionsMethod       :: String
  , _createIndexOptionsOutputIbFile :: Maybe FilePath
  , _createIndexOptionsOutputBpFile :: Maybe FilePath
  } deriving (Eq, Show)

data DemoOptions = DemoOptions
  { _demoOptionsFilePath :: FilePath
  , _demoOptionsMethod   :: FilePath
  } deriving (Eq, Show)
