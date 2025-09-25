{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Criterion.Main
import Data.Text
import HaskellWorks.Data.Json.LightJson
import HaskellWorks.Data.Positioning

import qualified Data.ByteString                             as BS
import qualified Data.List                                   as L
import qualified HaskellWorks.Data.ByteString                as BS
import qualified HaskellWorks.Data.Json.Standard.Cursor.Fast as JCF
import qualified System.Directory                            as IO

countLightJsonEntryNodes :: [(Text, JCF.Cursor)] -> Count
countLightJsonEntryNodes _ = 0

countLightJsonNodes :: LightJson JCF.Cursor -> Count
countLightJsonNodes j = case j of
  LightJsonString _   -> 1
  LightJsonNumber _   -> 1
  LightJsonObject kvs -> 1 + sum (fmap (countLightJsonNodes . lightJsonAt . snd ) kvs )
  LightJsonArray es   -> 1 + sum (fmap (countLightJsonNodes . lightJsonAt       ) es  )
  LightJsonBool _     -> 1
  LightJsonNull       -> 1
  LightJsonError _    -> 1

getBenchmarkJsonFiles :: IO [FilePath]
getBenchmarkJsonFiles = do
  entries <- IO.listDirectory "data/bench"
  return $ L.sort (("data/bench/" ++) <$> (".json" `L.isSuffixOf`) `L.filter` entries)

runCount :: BS.ByteString -> Count
runCount bs = countLightJsonNodes (lightJsonAt c)
  where ibip = JCF.simdToIbBp bs
        c    = JCF.fromBsIbBp bs ibip

mkBenchCount :: [FilePath] -> IO Benchmark
mkBenchCount files = do
  benchmarks <- forM files $ \file -> return (env (BS.mmap file) $ \bs -> bench file (whnf runCount bs))
  return $ bgroup "Count nodes" benchmarks

main :: IO ()
main = do
  benchmarkFiles <- getBenchmarkJsonFiles
  benchmarks <- sequence
    [ mkBenchCount benchmarkFiles
    ]
  defaultMain benchmarks
