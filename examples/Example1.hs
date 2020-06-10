{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fno-warn-unused-matches     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing     #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}

module Example1 where

import Data.Aeson

import qualified Data.ByteString.Lazy as LBS

example :: IO ()
example = do
  !bs <- LBS.readFile "corpus/bench/hospitalisation.json"
  let !y = decode bs :: Maybe Value
  return ()
