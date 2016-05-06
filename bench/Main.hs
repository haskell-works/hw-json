{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Criterion.Main
import qualified Data.ByteString                                     as BS
import qualified Data.ByteString.Internal                            as BSI
import qualified Data.Vector.Storable                                as DVS
import           Data.Word
import           Foreign
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Json.Succinct.Cursor
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic
import           System.IO.MMap

setupEnvBs :: Int -> IO BS.ByteString
setupEnvBs n = return $ BS.pack (take n (cycle [maxBound, 0]))

setupEnvBss :: Int -> Int -> IO [BS.ByteString]
setupEnvBss n k = setupEnvBs n >>= \v -> return (replicate k v)

setupEnvVector :: Int -> IO (DVS.Vector Word64)
setupEnvVector n = return $ DVS.fromList (take n (cycle [maxBound, 0]))

setupEnvVectors :: Int -> Int -> IO [DVS.Vector Word64]
setupEnvVectors n k = setupEnvVector n >>= \v -> return (replicate k v)

setupEnvJson :: FilePath -> IO BS.ByteString
setupEnvJson filepath = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr filepath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  return bs

loadJson :: BS.ByteString -> JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
loadJson bs = fromByteString bs :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))

benchRankSelect :: [Benchmark]
benchRankSelect =
  [ env (setupEnvVector 1000000) $ \bv -> bgroup "Rank"
    [ bench "Rank - Once"   (whnf (rank1    bv) 1)
    , bench "Select - Once" (whnf (select1  bv) 1)
    , bench "Rank - Many"   (nf   (map (getCount . rank1  bv)) [0, 1000..10000000])
    ]
  ]

benchRankJson40Conduits :: [Benchmark]
benchRankJson40Conduits =
  [ env (setupEnvJson "/Users/jky/Downloads/part40.json") $ \bs -> bgroup "Json40"
    [ bench "loadJson                         "  (whnf  loadJson                            bs)
    ]
  ]

benchRankJson80Conduits :: [Benchmark]
benchRankJson80Conduits =
  [ env (setupEnvJson "/Users/jky/Downloads/part80.json") $ \bs -> bgroup "Json40"
    [ bench "loadJson" (whnf loadJson bs)
    ]
  ]

benchRankJsonBigConduits :: [Benchmark]
benchRankJsonBigConduits =
  [ env (setupEnvJson "/Users/jky/Downloads/78mb.json") $ \bs -> bgroup "JsonBig"
    [ bench "loadJson" (whnf loadJson bs)
    ]
  ]

benchBlankedJsonToBalancedParens :: [Benchmark]
benchBlankedJsonToBalancedParens =
  [ env (setupEnvJson "/Users/jky/Downloads/part40.json") $ \bs -> bgroup "JsonBig"
    [ bench "loadJson" (whnf loadJson bs)
    ]
  ]

main :: IO ()
main = defaultMain benchRankJson40Conduits
