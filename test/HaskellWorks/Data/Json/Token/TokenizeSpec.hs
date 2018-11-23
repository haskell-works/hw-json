{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Token.TokenizeSpec (spec) where

import Data.ByteString                                                 (ByteString)
import HaskellWorks.Data.Json.Internal.Backend.Standard.Token.Tokenize
import Test.Hspec

import qualified Data.Attoparsec.ByteString.Char8 as BC

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

parseJsonToken' :: ByteString -> Either String (JsonToken String Double)
parseJsonToken' = BC.parseOnly parseJsonToken

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Token.TokenizeSpec" $ do
  describe "When parsing single token at beginning of text" $ do
    it "Empty Json should produce no bits" $
      parseJsonToken' "" `shouldBe` Left "not enough input"
    it "Json with one space should produce whitespace token" $
      parseJsonToken' " " `shouldBe` Right JsonTokenWhitespace
    it "Json with two spaces should produce whitespace token" $
      parseJsonToken' "  " `shouldBe` Right JsonTokenWhitespace
    it "Spaces and newlines should produce no bits" $
      parseJsonToken' "  \n \r \t " `shouldBe` Right JsonTokenWhitespace
    it "`null` at beginning should produce one bit" $
      parseJsonToken' "null " `shouldBe` Right JsonTokenNull
    it "number at beginning should produce one bit" $
      parseJsonToken' "1234 " `shouldBe` Right (JsonTokenNumber 1234)
    it "false at beginning should produce one bit" $
      parseJsonToken' "false " `shouldBe` Right (JsonTokenBoolean False)
    it "true at beginning should produce one bit" $
      parseJsonToken' "true " `shouldBe` Right (JsonTokenBoolean True)
    it "string at beginning should produce one bit" $
      parseJsonToken' "\"hello\" " `shouldBe` Right (JsonTokenString "hello")
    it "quoted string should parse" $
      parseJsonToken' "\"\\\"\" " `shouldBe` Right (JsonTokenString "\"")
    it "left brace at beginning should produce one bit" $
      parseJsonToken' "{ " `shouldBe` Right JsonTokenBraceL
    it "right brace at beginning should produce one bit" $
      parseJsonToken' "} " `shouldBe` Right JsonTokenBraceR
    it "left bracket at beginning should produce one bit" $
      parseJsonToken' "[ " `shouldBe` Right JsonTokenBracketL
    it "right bracket at beginning should produce one bit" $
      parseJsonToken' "] " `shouldBe` Right JsonTokenBracketR
    it "right bracket at beginning should produce one bit" $
      parseJsonToken' ": " `shouldBe` Right JsonTokenColon
    it "right bracket at beginning should produce one bit" $
      parseJsonToken' ", " `shouldBe` Right JsonTokenComma
