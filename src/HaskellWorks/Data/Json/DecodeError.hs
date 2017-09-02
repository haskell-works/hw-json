module HaskellWorks.Data.Json.DecodeError where

newtype DecodeError = DecodeError String deriving (Eq, Show)
