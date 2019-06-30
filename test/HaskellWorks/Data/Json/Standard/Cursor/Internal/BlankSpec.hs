{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Standard.Cursor.Internal.BlankSpec (spec) where

import HaskellWorks.Data.Json.Standard.Cursor.Internal.Blank
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString as BS

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

whenBlankedJsonShouldBe :: BS.ByteString -> BS.ByteString -> Spec
whenBlankedJsonShouldBe original expected = do
  it (show original ++ " when blanked json should be " ++ show expected) $ requireTest $ do
    BS.concat (blankJson [original]) === expected

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Internal.BlankSpec" $ do
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
    " \"\"\"\" "                          `whenBlankedJsonShouldBe` " ()() "
    " [][] "                              `whenBlankedJsonShouldBe` " [][] "
    " {}{} "                              `whenBlankedJsonShouldBe` " {}{} "
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
