{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module HaskellWorks.Data.Json.Backend.Standard.Succinct.Cursor.BalancedParensSpec
  ( spec
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitShow
import Test.Hspec

import qualified Data.ByteString                                   as BS
import qualified Data.Vector.Storable                              as DVS
import qualified HaskellWorks.Data.Json.Backend.Standard.SemiIndex as SI

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

balancedParensOf2 :: BS.ByteString -> DVS.Vector Word64
balancedParensOf2 bs = let SI.SemiIndex _ bp = SI.buildSemiIndex bs in bp

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.Cursor.InterestBitsSpec" $ do
  it "Evaluating interest bits 2" $ do
    bitShow (balancedParensOf2 ""           ) `shouldBe` ""
    bitShow (balancedParensOf2 "  \n \r \t ") `shouldBe` ""
    bitShow (balancedParensOf2 "1234 "      ) `shouldBe` ""
    bitShow (balancedParensOf2 "false "     ) `shouldBe` ""
    bitShow (balancedParensOf2 "true "      ) `shouldBe` ""
    bitShow (balancedParensOf2 "\"hello\" " ) `shouldBe` ""
    bitShow (balancedParensOf2 "\"\\\"\" "  ) `shouldBe` ""
    bitShow (balancedParensOf2 "{ "         ) `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (balancedParensOf2 "} "         ) `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (balancedParensOf2 "[ "         ) `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (balancedParensOf2 "] "         ) `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (balancedParensOf2 ": "         ) `shouldBe` ""
    bitShow (balancedParensOf2 ", "         ) `shouldBe` ""
    bitShow (balancedParensOf2 "{{}}"       ) `shouldBe` "11000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (balancedParensOf2 " { { } } "  ) `shouldBe` "11000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
