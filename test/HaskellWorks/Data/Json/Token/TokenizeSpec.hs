{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Token.TokenizeSpec (spec) where

import Data.ByteString                                         (ByteString)
import HaskellWorks.Data.Json.Internal.Standard.Token.Tokenize
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Attoparsec.ByteString.Char8 as BC

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

parseJsonToken' :: ByteString -> Either String (JsonToken String Double)
parseJsonToken' = BC.parseOnly parseJsonToken

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Token.TokenizeSpec" $ do
  describe "When parsing single token at beginning of text" $ do
    it "Empty Json should produce no bits" $ requireTest $
      parseJsonToken' "" === Left "not enough input"
    it "Json with one space should produce whitespace token" $ requireTest $
      parseJsonToken' " " === Right JsonTokenWhitespace
    it "Json with two spaces should produce whitespace token" $ requireTest $
      parseJsonToken' "  " === Right JsonTokenWhitespace
    it "Spaces and newlines should produce no bits" $ requireTest $
      parseJsonToken' "  \n \r \t " === Right JsonTokenWhitespace
    it "`null` at beginning should produce one bit" $ requireTest $
      parseJsonToken' "null " === Right JsonTokenNull
    it "number at beginning should produce one bit" $ requireTest $
      parseJsonToken' "1234 " === Right (JsonTokenNumber 1234)
    it "false at beginning should produce one bit" $ requireTest $
      parseJsonToken' "false " === Right (JsonTokenBoolean False)
    it "true at beginning should produce one bit" $ requireTest $
      parseJsonToken' "true " === Right (JsonTokenBoolean True)
    it "string at beginning should produce one bit" $ requireTest $
      parseJsonToken' "\"hello\" " === Right (JsonTokenString "hello")
    it "quoted string should parse" $ requireTest $
      parseJsonToken' "\"\\\"\" " === Right (JsonTokenString "\"")
    it "left brace at beginning should produce one bit" $ requireTest $
      parseJsonToken' "{ " === Right JsonTokenBraceL
    it "right brace at beginning should produce one bit" $ requireTest $
      parseJsonToken' "} " === Right JsonTokenBraceR
    it "left bracket at beginning should produce one bit" $ requireTest $
      parseJsonToken' "[ " === Right JsonTokenBracketL
    it "right bracket at beginning should produce one bit" $ requireTest $
      parseJsonToken' "] " === Right JsonTokenBracketR
    it "right bracket at beginning should produce one bit" $ requireTest $
      parseJsonToken' ": " === Right JsonTokenColon
    it "right bracket at beginning should produce one bit" $ requireTest $
      parseJsonToken' ", " === Right JsonTokenComma
