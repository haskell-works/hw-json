{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main

main :: IO ()
main = do
  benchmarks <- mconcat <$> sequence mempty
  defaultMain benchmarks
