{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main
import Data.Word
import Foreign
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.Json.Internal.Blank
import HaskellWorks.Data.Json.Internal.MakeIndex
import HaskellWorks.Data.Json.Succinct.Cursor.Internal
import System.IO.MMap

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Storable     as DVS

setupEnvJson :: FilePath -> IO BS.ByteString
setupEnvJson filepath = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr filepath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  return bs

loadJson :: BS.ByteString -> JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
loadJson bs = fromByteString bs :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))

jsonToInterestBits3 :: [BS.ByteString] -> [BS.ByteString]
jsonToInterestBits3 = blankedJsonToInterestBits . blankJson

benchRankJson40 :: [Benchmark]
benchRankJson40 =
  [ env (setupEnvJson "corpus/part40.json") $ \bs -> bgroup "Json40"
    [ bench "Run blankJson                    "  (whnf (BS.concat . blankJson          ) [bs])
    , bench "Run jsonToInterestBits3          "  (whnf (BS.concat . jsonToInterestBits3) [bs])
    , bench "loadJson                         "  (whnf loadJson                           bs )
    ]
  ]

benchRankJson80 :: [Benchmark]
benchRankJson80 =
  [ env (setupEnvJson "corpus/part80.json") $ \bs -> bgroup "Json40"
    [ bench "Run blankJson                    "  (whnf (BS.concat . blankJson          ) [bs])
    , bench "Run jsonToInterestBits3          "  (whnf (BS.concat . jsonToInterestBits3) [bs])
    , bench "loadJson" (whnf loadJson bs)
    ]
  ]

benchRankJsonBig :: [Benchmark]
benchRankJsonBig =
  [ env (setupEnvJson "corpus/78mb.json") $ \bs -> bgroup "JsonBig"
    [ bench "Run blankJson                    "  (whnf (BS.concat . blankJson          ) [bs])
    , bench "Run jsonToInterestBits3          "  (whnf (BS.concat . jsonToInterestBits3) [bs])
    , bench "loadJson" (whnf loadJson bs)
    ]
  ]

main :: IO ()
main = defaultMain benchRankJson40
