module App.Commands.Types
  ( CreateIndexOptions(..)
  , DemoOptions(..)
  ) where

data CreateIndexOptions = CreateIndexOptions
  { _createIndexOptionsFilePath     :: FilePath
  , _createIndexOptionsBackend      :: String
  , _createIndexOptionsOutputIbFile :: Maybe FilePath
  , _createIndexOptionsOutputBpFile :: Maybe FilePath
  } deriving (Eq, Show)

newtype DemoOptions = DemoOptions
  { _demoOptionsFilePath  :: FilePath
  } deriving (Eq, Show)
