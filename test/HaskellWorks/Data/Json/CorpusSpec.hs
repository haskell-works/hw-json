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

import Control.Monad.IO.Class
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.FromBitTextByteString
import HaskellWorks.Data.Json.Backend.Standard.Cursor
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString                              as BS
import qualified HaskellWorks.Data.Json.Backend.Standard.Slow as SLOW

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Corpus" $ do
  it "Corpus 5000B loads properly" $ requireTest $ do
    inJsonBS                    <- liftIO $ BS.readFile "corpus/5000B.json"
    inInterestBitsBS            <- liftIO $ BS.readFile "corpus/5000B.json.ib.idx"
    inInterestBalancedParensBS  <- liftIO $ BS.readFile "corpus/5000B.json.bp.idx"
    let inInterestBits            = fromBitTextByteString inInterestBitsBS
    let inInterestBalancedParens  = fromBitTextByteString inInterestBalancedParensBS
    let !cursor = SLOW.makeCursor inJsonBS
    let text                    = cursorText      cursor
    let ib                      = interests       cursor
    let SimpleBalancedParens bp = balancedParens  cursor
    text === inJsonBS
    ib === inInterestBits
    bp === inInterestBalancedParens
  it "issue-0001 loads properly" $ requireTest $ do
    inJsonBS                    <- liftIO $ BS.readFile "corpus/issue-0001.json"
    inInterestBitsBS            <- liftIO $ BS.readFile "corpus/issue-0001.json.ib.idx"
    inInterestBalancedParensBS  <- liftIO $ BS.readFile "corpus/issue-0001.json.bp.idx"
    let inInterestBits            = fromBitTextByteString inInterestBitsBS
    let inInterestBalancedParens  = fromBitTextByteString inInterestBalancedParensBS
    let !cursor = SLOW.makeCursor inJsonBS
    let text                    = cursorText      cursor
    let ib                      = interests       cursor
    let SimpleBalancedParens bp = balancedParens  cursor
    text === inJsonBS
    ib === inInterestBits
    bp === inInterestBalancedParens
