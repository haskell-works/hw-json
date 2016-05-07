{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.ExtractSpec (spec) where

import           HaskellWorks.Data.Json.Extract
import           HaskellWorks.Data.Json.Type
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Json.ExtractSpec" $ do
  describe "When extracting type and string" $ do
    it "should work for empty string only" $
      extractJsonSnippet "\"\"" `shouldBe` Just (JsonTypeString, "\"\"")
    it "should work for empty string followed by space" $
      extractJsonSnippet "\"\" " `shouldBe` Just (JsonTypeString, "\"\"")
    it "should work for empty string followed by double quote" $
      extractJsonSnippet "\"\"\"" `shouldBe` Just (JsonTypeString, "\"\"")
    it "should work for empty string followed by comma" $
      extractJsonSnippet "\"\"," `shouldBe` Just (JsonTypeString, "\"\"")
    it "should work for string with escaped double quotes" $
      extractJsonSnippet "\"\\\"\"," `shouldBe` Just (JsonTypeString, "\"\\\"\"")
    it "should work for string with escaped double quotes and spaces" $
      extractJsonSnippet "\" \\\" \"," `shouldBe` Just (JsonTypeString, "\" \\\" \"")
    it "should work for null" $
      extractJsonSnippet "null, 1" `shouldBe` Just (JsonTypeNull, "n")
    it "should work for true" $
      extractJsonSnippet "true, 1" `shouldBe` Just (JsonTypeBool, "t")
    it "should work for false" $
      extractJsonSnippet "false, 1" `shouldBe` Just (JsonTypeBool, "f")
    it "should work for number only" $
      extractJsonSnippet "1.0e-2" `shouldBe` Just (JsonTypeNumber, "1.0e-2")
    it "should work for number followed by space" $
      extractJsonSnippet "-123.02e+2 " `shouldBe` Just (JsonTypeNumber, "-123.02e+2")
    it "should work for array" $
      extractJsonSnippet "[1, 2, 3] " `shouldBe` Just (JsonTypeArray, "[")
    it "should work for object" $
      extractJsonSnippet "{\"field\": 1}" `shouldBe` Just (JsonTypeObject, "{")
