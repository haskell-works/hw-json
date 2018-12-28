{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Succinct.Cursor.BalancedParensSpec(spec) where

import Data.String
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Json.Internal.Backend.Standard.MakeIndex
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString                                              as BS
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.BlankedJson as J

{-# ANN module ("HLint: Ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.Cursor.BalancedParensSpec" $ do
  it "Blanking JSON should not contain strange characters 1" $ requireTest $ do
    let blankedJson = J.BlankedJson ["[ [],", "[]]"]
    let bp = BitShown $ BS.concat (blankedJsonToBalancedParens (J.unBlankedJson blankedJson))
    bp === fromString "11111111 11111111 00000000 11111111 00000000 00000000"
  it "Blanking JSON should not contain strange characters 2" $ requireTest $ do
    let blankedJson = J.BlankedJson ["[ [],", "[]]"]
    let bp = BitShown $ BS.concat ((compressWordAsBit . blankedJsonToBalancedParens) (J.unBlankedJson blankedJson))
    bp === fromString "11010000"
