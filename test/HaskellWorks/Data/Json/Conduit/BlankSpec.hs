{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Conduit.BlankSpec (spec) where

import qualified Data.ByteString                      as BS
import           HaskellWorks.Data.Json.Conduit.Blank
import           HaskellWorks.Data.Conduit.List
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

whenBlankedJsonShouldBe :: BS.ByteString -> BS.ByteString -> Spec
whenBlankedJsonShouldBe original expected = do
  it (show original ++ " when blanked json should be " ++ show expected) $ do
    BS.concat (runListConduit blankJson [original]) `shouldBe` expected

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Conduit.BlankSpec" $ do
  describe "Can blank json" $ do
    "\"\""                                `whenBlankedJsonShouldBe` "()"
    "\"\\\\\""                            `whenBlankedJsonShouldBe` "(  )"
    "\"\\\\\\\""                          `whenBlankedJsonShouldBe` "(    "
    "\" \\\\\\\""                         `whenBlankedJsonShouldBe` "(     "
    "\" \\n\\\\\""                        `whenBlankedJsonShouldBe` "(     )"
    ""                                    `whenBlankedJsonShouldBe` ""
    "\"\""                                `whenBlankedJsonShouldBe` "()"
    "\" \""                               `whenBlankedJsonShouldBe` "( )"
    "\" a \""                             `whenBlankedJsonShouldBe` "(   )"
    " \"a \" x"                           `whenBlankedJsonShouldBe` " (  ) x"
    " \"a\"b\"c\"d"                       `whenBlankedJsonShouldBe` " ( )b( )d"
    ""                                    `whenBlankedJsonShouldBe` ""
    "1"                                   `whenBlankedJsonShouldBe` "1"
    "11"                                  `whenBlankedJsonShouldBe` "10"
    "00"                                  `whenBlankedJsonShouldBe` "10"
    "00"                                  `whenBlankedJsonShouldBe` "10"
    "-0.12e+34"                           `whenBlankedJsonShouldBe` "100000000"
    "10.12E-34 "                          `whenBlankedJsonShouldBe` "100000000 "
    "10.12E-34 12"                        `whenBlankedJsonShouldBe` "100000000 10"
    " 10.12E-34 -1"                       `whenBlankedJsonShouldBe` " 100000000 10"
    ""                                    `whenBlankedJsonShouldBe` ""
    "a"                                   `whenBlankedJsonShouldBe` "a"
    "z"                                   `whenBlankedJsonShouldBe` "z"
    " Aaa "                               `whenBlankedJsonShouldBe` " A__ "
    " Za def "                            `whenBlankedJsonShouldBe` " Z_ d__ "
    ""                                    `whenBlankedJsonShouldBe` ""
    " { \"ff\": 1.0, [\"\", true], null}" `whenBlankedJsonShouldBe` " { (  ): 100, [(), t___], n___}"
