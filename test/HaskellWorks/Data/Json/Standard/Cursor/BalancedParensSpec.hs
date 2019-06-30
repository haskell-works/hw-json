{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Standard.Cursor.BalancedParensSpec
  ( spec
  ) where

import Data.String
import Data.Word
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Json.Standard.Cursor.Internal.MakeIndex
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString                                             as BS
import qualified Data.Vector.Storable                                        as DVS
import qualified HaskellWorks.Data.Json.Standard.Cursor.Internal.BlankedJson as J
import qualified HaskellWorks.Data.Json.Standard.Cursor.SemiIndex            as SI

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

balancedParensOf2 :: BS.ByteString -> DVS.Vector Word64
balancedParensOf2 bs = let SI.SemiIndex _ bp = SI.buildSemiIndex bs in bp

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Cursor.InterestBitsSpec" $ do
  it "Evaluating interest bits 2" $ requireTest $ do
    bitShow (balancedParensOf2 ""           ) === ""
    bitShow (balancedParensOf2 "  \n \r \t ") === ""
    bitShow (balancedParensOf2 "1234 "      ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (balancedParensOf2 "1.1 "       ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (balancedParensOf2 "-1.1e-1 "   ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (balancedParensOf2 "false "     ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (balancedParensOf2 "true "      ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (balancedParensOf2 "\"hello\" " ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (balancedParensOf2 "\"\\\"\" "  ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (balancedParensOf2 "{ "         ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (balancedParensOf2 "} "         ) === "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (balancedParensOf2 "[ "         ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (balancedParensOf2 "] "         ) === "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (balancedParensOf2 ": "         ) === ""
    bitShow (balancedParensOf2 ", "         ) === ""
    bitShow (balancedParensOf2 "{{}}"       ) === "11000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (balancedParensOf2 " { { } } "  ) === "11000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
  it "Blanking JSON should not contain strange characters 1" $ requireTest $ do
    let blankedJson = J.BlankedJson ["[ [],", "[]]"]
    let bp = BitShown $ BS.concat (blankedJsonToBalancedParens (J.unBlankedJson blankedJson))
    bp === fromString "11111111 11111111 00000000 11111111 00000000 00000000"
  it "Blanking JSON should not contain strange characters 2" $ requireTest $ do
    let blankedJson = J.BlankedJson ["[ [],", "[]]"]
    let bp = BitShown $ BS.concat ((compressWordAsBit . blankedJsonToBalancedParens) (J.unBlankedJson blankedJson))
    bp === fromString "11010000"
