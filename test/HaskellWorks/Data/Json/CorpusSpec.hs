{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Json.CorpusSpec(spec) where

import qualified Data.ByteString                                            as BS
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Json.Succinct.Cursor                     as C
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           Test.Hspec

import           HaskellWorks.Data.FromByteString

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Corpus" $ do
  it "Corpus 5000B loads properly" $ do
    inJson <- BS.readFile "corpus/5000B.json"
    inInterestBits <- BS.readFile "corpus/5000B.ib"
    inInterestBalancedParens <- BS.readFile "corpus/5000B.bp"
    let !cursor = fromByteString inJson :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
    let text = cursorText cursor
    let ib = interests cursor
    let bp = balancedParens cursor
    text `shouldBe` inJson
