{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Criterion.Main
import Data.List
import Data.Word
import Foreign
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.Json.Cursor
import HaskellWorks.Data.Json.Internal.Blank
import HaskellWorks.Data.Json.Internal.MakeIndex
import System.IO.MMap

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Storable     as DVS
import qualified System.Directory         as IO

setupEnvJson :: FilePath -> IO BS.ByteString
setupEnvJson filepath = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr filepath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  return bs

loadJson :: BS.ByteString -> JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
loadJson bs = fromByteString bs :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))

jsonToInterestBits3 :: [BS.ByteString] -> [BS.ByteString]
jsonToInterestBits3 = blankedJsonToInterestBits . blankJson

makeBenchBlankJson :: IO [Benchmark]
makeBenchBlankJson = do
  entries <- IO.listDirectory "corpus/bench"
  let files = ("corpus/bench/" ++) <$> (".json" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (setupEnvJson file) $ \bs -> bgroup file
      [ bench "Run blankJson" (whnf (BS.concat . blankJson) [bs])
      ]
    ]

  return (join benchmarks)

makeBenchJsonToInterestBits :: IO [Benchmark]
makeBenchJsonToInterestBits = do
  entries <- IO.listDirectory "corpus/bench"
  let files = ("corpus/bench/" ++) <$> (".json" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (setupEnvJson file) $ \bs -> bgroup file
      [ bench "Run blankJson" (whnf (BS.concat . jsonToInterestBits3) [bs])
      ]
    ]

  return (join benchmarks)

makeBenchLoadJson :: IO [Benchmark]
makeBenchLoadJson = do
  entries <- IO.listDirectory "corpus/bench"
  let files = ("corpus/bench/" ++) <$> (".json" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (setupEnvJson file) $ \bs -> bgroup file
      [ bench "Run blankJson" (whnf loadJson bs)
      ]
    ]

  return (join benchmarks)

main :: IO ()
main = do
  benchmarks <- mconcat <$> sequence
    [ makeBenchBlankJson
    , makeBenchJsonToInterestBits
    , makeBenchLoadJson
    ]
  defaultMain benchmarks
