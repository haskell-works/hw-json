{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Json.CorpusSpec(spec) where

import Data.Word
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.FromBitTextByteString
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.Json.Cursor
import Test.Hspec

import qualified Data.ByteString                              as BS
import qualified Data.Vector.Storable                         as DVS
import qualified HaskellWorks.Data.Json.Backend.Standard.Slow as SLOW

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Corpus" $ do
  it "Corpus 5000B loads properly" $ do
    inJsonBS                    <- BS.readFile "corpus/5000B.json"
    inInterestBitsBS            <- BS.readFile "corpus/5000B.ib"
    inInterestBalancedParensBS  <- BS.readFile "corpus/5000B.bp"
    let inInterestBits            = fromBitTextByteString inInterestBitsBS
    let inInterestBalancedParens  = fromBitTextByteString inInterestBalancedParensBS
    let !cursor = SLOW.makeCursor inJsonBS
    let text                    = cursorText      cursor
    let ib                      = interests       cursor
    let SimpleBalancedParens bp = balancedParens  cursor
    text `shouldBe` inJsonBS
    ib `shouldBe` inInterestBits
    bp `shouldBe` inInterestBalancedParens
  it "issue-0001 loads properly" $ do
    inJsonBS                    <- BS.readFile "corpus/issue-0001.json"
    inInterestBitsBS            <- BS.readFile "corpus/issue-0001.ib"
    inInterestBalancedParensBS  <- BS.readFile "corpus/issue-0001.bp"
    let inInterestBits            = fromBitTextByteString inInterestBitsBS
    let inInterestBalancedParens  = fromBitTextByteString inInterestBalancedParensBS
    let !cursor = SLOW.makeCursor inJsonBS
    let text                    = cursorText      cursor
    let ib                      = interests       cursor
    let SimpleBalancedParens bp = balancedParens  cursor
    text `shouldBe` inJsonBS
    ib `shouldBe` inInterestBits
    bp `shouldBe` inInterestBalancedParens
