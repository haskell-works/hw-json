{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module HaskellWorks.Data.Json.Backend.Standard.Succinct.Cursor.InterestBitsSpec(spec) where

import Data.String
import Data.Word
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitShown
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
  it "Evaluating interest bits" $ do
    BitShown (interestBitsOf ""           ) `shouldBe` fromString ""
    BitShown (interestBitsOf "  \n \r \t ") `shouldBe` fromString "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "1234 "      ) `shouldBe` fromString "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "false "     ) `shouldBe` fromString "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "true "      ) `shouldBe` fromString "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "\"hello\" " ) `shouldBe` fromString "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "\"\\\"\" "  ) `shouldBe` fromString "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "{ "         ) `shouldBe` fromString "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "} "         ) `shouldBe` fromString "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "[ "         ) `shouldBe` fromString "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "] "         ) `shouldBe` fromString "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf ": "         ) `shouldBe` fromString "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf ", "         ) `shouldBe` fromString "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf "{{}}"       ) `shouldBe` fromString "11000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    BitShown (interestBitsOf " { { } } "  ) `shouldBe` fromString "01010000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
  it "Evaluating interest bits 2" $ do
    bitShow (interestBitsOf2 ""           ) `shouldBe` ""
    bitShow (interestBitsOf2 "  \n \r \t ") `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "1234 "      ) `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "false "     ) `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "true "      ) `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "\"hello\" " ) `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "\"\\\"\" "  ) `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "{ "         ) `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "} "         ) `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "[ "         ) `shouldBe` "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "] "         ) `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 ": "         ) `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 ", "         ) `shouldBe` "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 "{{}}"       ) `shouldBe` "11000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (interestBitsOf2 " { { } } "  ) `shouldBe` "01010000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
