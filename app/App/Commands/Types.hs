{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Types
  ( CreateIndexOptions(..)
  , DemoOptions(..)
  ) where

newtype CreateIndexOptions = CreateIndexOptions
  { _createIndexOptionsFilePath  :: FilePath
  } deriving (Eq, Show)

newtype DemoOptions = DemoOptions
  { _demoOptionsFilePath  :: FilePath
  } deriving (Eq, Show)
