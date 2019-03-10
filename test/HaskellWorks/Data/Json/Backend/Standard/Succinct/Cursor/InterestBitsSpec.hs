{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module HaskellWorks.Data.Json.Backend.Standard.Succinct.Cursor.InterestBitsSpec(spec) where

import Data.String
import Data.Word
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString                                                   as BS
import qualified Data.Vector.Storable                                              as DVS
import qualified HaskellWorks.Data.Json.Backend.Standard.SemiIndex                 as SI
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.BlankedJson      as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.ToInterestBits64 as J

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

interestBitsOf :: BS.ByteString -> DVS.Vector Word64
interestBitsOf bs = J.toInterestBits64 (J.toBlankedJsonTyped bs)

interestBitsOf2 :: BS.ByteString -> DVS.Vector Word64
interestBitsOf2 bs = let SI.SemiIndex ib _ = SI.buildSemiIndex bs in ib

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.Cursor.InterestBitsSpec" $ do
  it "Evaluating interest bits" $ requireTest $ do
    BitShown (interestBitsOf ""           ) === fromString ""
    BitShown (interestBitsOf "  \n \r \t ") === fromString "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "1234 "      ) === fromString "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "1.1 "       ) === fromString "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "-1.1e-2 "   ) === fromString "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "false "     ) === fromString "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "true "      ) === fromString "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "\"hello\" " ) === fromString "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "\"\\\"\" "  ) === fromString "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "{ "         ) === fromString "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "} "         ) === fromString "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "[ "         ) === fromString "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "] "         ) === fromString "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf ": "         ) === fromString "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf ", "         ) === fromString "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "{{}}"       ) === fromString "11000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf " { { } } "  ) === fromString "01010000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
  it "Evaluating interest bits 2" $ requireTest $ do
    bitShow (interestBitsOf2 ""           ) === ""
    bitShow (interestBitsOf2 "  \n \r \t ") === "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "1234 "      ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "1.1 "       ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "-1.1 "      ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "-1.1e-2 "   ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "false "     ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "true "      ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "\"hello\" " ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "\"\\\"\" "  ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "{ "         ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "} "         ) === "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "[ "         ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "] "         ) === "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 ": "         ) === "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 ", "         ) === "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "{{}}"       ) === "11000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 " { { } } "  ) === "01010000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
