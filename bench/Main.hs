{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main

main :: IO ()
main = do
  benchmarks <- fmap mconcat $ sequence $ mempty
  defaultMain benchmarks
