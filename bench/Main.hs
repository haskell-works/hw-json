{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Criterion.Main
import           Control.Monad.Trans.Resource                        (MonadThrow)
import qualified Data.ByteString                                     as BS
import qualified Data.ByteString.Internal                            as BSI
import           Data.Conduit
import qualified Data.Vector.Storable                                as DVS
import           Data.Word
import           Foreign
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Json.Conduit
import           HaskellWorks.Data.Json.Conduit.Blank
import           HaskellWorks.Data.Json.Succinct.Cursor
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           System.IO.MMap

setupEnvJson :: FilePath -> IO BS.ByteString
setupEnvJson filepath = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr filepath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  return bs

loadJson :: BS.ByteString -> JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
loadJson bs = fromByteString bs :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))

runCon :: Conduit i [] BS.ByteString -> i -> BS.ByteString
runCon con bs = BS.concat $ runListConduit con [bs]

jsonToInterestBits3 :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
jsonToInterestBits3 = blankJson =$= blankedJsonToInterestBits

benchRankJson40Conduits :: [Benchmark]
benchRankJson40Conduits =
  [ env (setupEnvJson "corpus/part40.json") $ \bs -> bgroup "Json40"
    [ bench "Run blankJson                    "  (whnf (runCon blankJson                  ) bs)
    , bench "Run jsonToInterestBits3          "  (whnf (runCon jsonToInterestBits3        ) bs)
    , bench "loadJson                         "  (whnf  loadJson                            bs)
    ]
  ]

benchRankJson80Conduits :: [Benchmark]
benchRankJson80Conduits =
  [ env (setupEnvJson "corpus/part80.json") $ \bs -> bgroup "Json40"
    [ bench "Run blankJson                    "  (whnf (runCon blankJson                  ) bs)
    , bench "Run jsonToInterestBits3          "  (whnf (runCon jsonToInterestBits3        ) bs)
    , bench "loadJson" (whnf loadJson bs)
    ]
  ]

benchRankJsonBigConduits :: [Benchmark]
benchRankJsonBigConduits =
  [ env (setupEnvJson "corpus/78mb.json") $ \bs -> bgroup "JsonBig"
    [ bench "Run blankJson                    "  (whnf (runCon blankJson                  ) bs)
    , bench "Run jsonToInterestBits3          "  (whnf (runCon jsonToInterestBits3        ) bs)
    , bench "loadJson" (whnf loadJson bs)
    ]
  ]

main :: IO ()
main = defaultMain benchRankJson40Conduits
