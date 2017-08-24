module HaskellWorks.Data.Json.DecodeError
  ( DecodeError(..)
  ) where

newtype DecodeError = DecodeError String deriving (Eq, Show)
