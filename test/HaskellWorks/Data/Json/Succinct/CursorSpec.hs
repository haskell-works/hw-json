{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Json.Succinct.CursorSpec(spec) where

import           Control.Monad
import qualified Data.ByteString                                            as BS
import qualified Data.Map                                                   as M
import           Data.String
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShow
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Json.Succinct.Cursor                     as C
import           HaskellWorks.Data.Json.Token
import           HaskellWorks.Data.Json.Value
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
import qualified HaskellWorks.Data.TreeCursor as TC
import           System.IO.MMap
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

type JsonValue = GenJsonValue BS.ByteString BS.ByteString

fc = TC.firstChild
ns = TC.nextSibling
pn = TC.parent
cd = TC.depth
ss = TC.subtreeSize

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.CursorSpec" $ do
  describe "Cursor for [Bool]" $ do
    it "initialises to beginning of empty object" $ do
      let cursor = "{}" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType cursor `shouldBe` Just JsonCursorObject
    it "initialises to beginning of empty object preceded by spaces" $ do
      let cursor = " {}" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType cursor `shouldBe` Just JsonCursorObject
    it "initialises to beginning of number" $ do
      let cursor = "1234" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType cursor `shouldBe` Just JsonCursorNumber
    it "initialises to beginning of string" $ do
      let cursor = "\"Hello\"" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType cursor `shouldBe` Just JsonCursorString
    it "initialises to beginning of array" $ do
      let cursor = "[]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType cursor `shouldBe` Just JsonCursorArray
    it "initialises to beginning of boolean true" $ do
      let cursor = "true" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType cursor `shouldBe` Just JsonCursorBool
    it "initialises to beginning of boolean false" $ do
      let cursor = "false" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType cursor `shouldBe` Just JsonCursorBool
    it "initialises to beginning of null" $ do
      let cursor = "null" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType cursor `shouldBe` Just JsonCursorNull
    it "cursor can navigate to first child of array" $ do
      let cursor = "[null]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> jsonCursorType) cursor `shouldBe` Just JsonCursorNull
    it "cursor can navigate to second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> ns >=> jsonCursorType) cursor `shouldBe` Just JsonCursorObject
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> ns >=> fc >=> jsonCursorType) cursor `shouldBe` Just JsonCursorString
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> ns >=> fc >=> ns >=> jsonCursorType) cursor `shouldBe` Just JsonCursorNumber
    it "depth at top" $ do
      let cursor = "[null]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd cursor `shouldBe` 1
    it "depth at first child of array" $ do
      let cursor = "[null]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd <$> fc cursor `shouldBe` Just 2
    it "depth at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd <$> (fc >=> ns) cursor `shouldBe` Just 2
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd <$> (fc >=> ns >=> fc) cursor `shouldBe` Just 3
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd <$> (fc >=> ns >=> fc >=> ns) cursor `shouldBe` Just 3
  genSpec "DVS.Vector Word8"  (undefined :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8)))
  genSpec "DVS.Vector Word16" (undefined :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16)))
  genSpec "DVS.Vector Word32" (undefined :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32)))
  genSpec "DVS.Vector Word64" (undefined :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
  genSpec "Poppy512"          (undefined :: JsonCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64)))
  it "Loads same Json consistentally from different backing vectors" $ do
    let cursor8   = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8))
    let cursor16  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16))
    let cursor32  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32))
    let cursor64  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
    cursorText cursor8 `shouldBe` cursorText cursor16
    cursorText cursor8 `shouldBe` cursorText cursor32
    cursorText cursor8 `shouldBe` cursorText cursor64
    let ic8   = bitShow $ interests cursor8
    let ic16  = bitShow $ interests cursor16
    let ic32  = bitShow $ interests cursor32
    let ic64  = bitShow $ interests cursor64
    ic16 `shouldBeginWith` ic8
    ic32 `shouldBeginWith` ic16
    ic64 `shouldBeginWith` ic32

shouldBeginWith :: (Eq a, Show a) => [a] -> [a] -> IO ()
shouldBeginWith as bs = take (length bs) as `shouldBe` bs

genSpec :: forall t u.
  ( Eq                t
  , Show              t
  , Select1           t
  , Eq                u
  , Show              u
  , Rank0             u
  , Rank1             u
  , BalancedParens    u
  , TestBit           u
  , FromForeignRegion (JsonCursor BS.ByteString t u)
  , IsString          (JsonCursor BS.ByteString t u)
  , HasJsonCursorType (JsonCursor BS.ByteString t u)
  , GenJsonValueAt BS.ByteString BS.ByteString (JsonCursor BS.ByteString t u))
  => String -> (JsonCursor BS.ByteString t u) -> SpecWith ()
genSpec t _ = do
  describe ("Cursor for (" ++ t ++ ")") $ do
    it "initialises to beginning of empty object" $ do
      let cursor = "{}" :: JsonCursor BS.ByteString t u
      jsonCursorType cursor `shouldBe` Just JsonCursorObject
      jsonValueAt cursor `shouldBe` Just (JsonObject M.empty :: JsonValue)
    it "initialises to beginning of empty object preceded by spaces" $ do
      let cursor = " {}" :: JsonCursor BS.ByteString t u
      jsonCursorType cursor `shouldBe` Just JsonCursorObject
      jsonValueAt cursor `shouldBe` Just (JsonObject M.empty :: JsonValue)
    it "initialises to beginning of number" $ do
      let cursor = "1234" :: JsonCursor BS.ByteString t u
      jsonCursorType cursor `shouldBe` Just JsonCursorNumber
      jsonValueAt cursor `shouldBe` Just (JsonNumber "1234" :: JsonValue)
    it "initialises to beginning of string" $ do
      let cursor = "\"Hello\"" :: JsonCursor BS.ByteString t u
      jsonCursorType cursor `shouldBe` Just JsonCursorString
      jsonValueAt cursor `shouldBe` Just (JsonString "\"Hello\"" :: JsonValue)
    it "initialises to beginning of array" $ do
      let cursor = "[]" :: JsonCursor BS.ByteString t u
      jsonCursorType cursor `shouldBe` Just JsonCursorArray
      jsonValueAt cursor `shouldBe` Just (JsonArray [] :: JsonValue)
    it "initialises to beginning of boolean true" $ do
      let cursor = "true" :: JsonCursor BS.ByteString t u
      jsonCursorType cursor `shouldBe` Just JsonCursorBool
      jsonValueAt cursor `shouldBe` Just (JsonBool True :: JsonValue)
    it "initialises to beginning of boolean false" $ do
      let cursor = "false" :: JsonCursor BS.ByteString t u
      jsonCursorType cursor `shouldBe` Just JsonCursorBool
      jsonValueAt cursor `shouldBe` Just (JsonBool False :: JsonValue)
    it "initialises to beginning of null" $ do
      let cursor = "null" :: JsonCursor BS.ByteString t u
      jsonCursorType cursor `shouldBe` Just JsonCursorNull
      jsonValueAt cursor `shouldBe` Just (JsonNull :: JsonValue)
    it "cursor can navigate to first child of array" $ do
      let cursor = "[null]" :: JsonCursor BS.ByteString t u
      (fc >=> jsonCursorType) cursor `shouldBe` Just JsonCursorNull
      jsonValueAt cursor `shouldBe` Just (JsonArray [JsonNull] :: JsonValue)
      ((fc >=> jsonValueAt) cursor) `shouldBe` Just (JsonNull :: JsonValue)
    it "cursor can navigate to second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t u
      (fc >=> ns >=> jsonCursorType) cursor `shouldBe` Just JsonCursorObject
      ((fc >=> ns >=> jsonValueAt) cursor) `shouldBe` Just (JsonObject (M.fromList [("\"field\"", JsonNumber "1")]) :: JsonValue)
      jsonValueAt cursor `shouldBe` Just (JsonArray [JsonNull, JsonObject (M.fromList [("\"field\"", JsonNumber "1")])] :: JsonValue)
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t u
      (fc >=> ns >=> fc >=> jsonCursorType) cursor `shouldBe` Just JsonCursorString
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t u
      (fc >=> ns >=> fc >=> ns >=> jsonCursorType) cursor `shouldBe` Just JsonCursorNumber
    it "depth at top" $ do
      let cursor = "[null]" :: JsonCursor BS.ByteString t u
      cd cursor `shouldBe` 1
    it "depth at first child of array" $ do
      let cursor = "[null]" :: JsonCursor BS.ByteString t u
      cd <$> (fc) cursor `shouldBe` Just 2
    it "depth at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t u
      cd <$> (fc >=> ns) cursor `shouldBe` Just 2
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t u
      cd <$> (fc >=> ns >=> fc) cursor `shouldBe` Just 3
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t u
      cd <$> (fc >=> ns >=> fc >=> ns) cursor `shouldBe` Just 3
    it "can navigate down and forwards" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/data/sample.json" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString t u
      (jsonCursorType                                                                                    ) cursor `shouldBe` Just JsonCursorObject
      (fc                                                                              >=> jsonCursorType) cursor `shouldBe` Just JsonCursorString
      (fc >=> ns                                                                       >=> jsonCursorType) cursor `shouldBe` Just JsonCursorObject
      (fc >=> ns >=> fc                                                                >=> jsonCursorType) cursor `shouldBe` Just JsonCursorString
      (fc >=> ns >=> fc >=> ns                                                         >=> jsonCursorType) cursor `shouldBe` Just JsonCursorString
      (fc >=> ns >=> fc >=> ns >=> ns                                                  >=> jsonCursorType) cursor `shouldBe` Just JsonCursorString
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns                                           >=> jsonCursorType) cursor `shouldBe` Just JsonCursorObject
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                                    >=> jsonCursorType) cursor `shouldBe` Just JsonCursorString
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns                             >=> jsonCursorType) cursor `shouldBe` Just JsonCursorString
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns                      >=> jsonCursorType) cursor `shouldBe` Just JsonCursorString
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns               >=> jsonCursorType) cursor `shouldBe` Just JsonCursorString
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns        >=> jsonCursorType) cursor `shouldBe` Just JsonCursorString
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns >=> ns >=> jsonCursorType) cursor `shouldBe` Just JsonCursorNumber
    it "can navigate up" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/data/sample.json" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString t u
      (fc >=> pn                                                                             ) cursor `shouldBe`                                    Just cursor
      (fc >=> ns >=> pn                                                                      ) cursor `shouldBe`                                    Just cursor
      (fc >=> ns >=> fc >=> pn                                                               ) cursor `shouldBe` (fc >=> ns                            ) cursor
      (fc >=> ns >=> fc >=> ns >=> pn                                                        ) cursor `shouldBe` (fc >=> ns                            ) cursor
      (fc >=> ns >=> fc >=> ns >=> ns >=> pn                                                 ) cursor `shouldBe` (fc >=> ns                            ) cursor
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> pn                                          ) cursor `shouldBe` (fc >=> ns                            ) cursor
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> pn                                   ) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> pn                            ) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> pn                     ) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> pn              ) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns >=> pn       ) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns >=> ns >=> pn) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
    it "can get subtree size" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/data/sample.json" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString t u
      ss                                                                                        cursor  `shouldBe` 45
      ss <$> (fc                                                                              ) cursor `shouldBe` Just 1
      ss <$> (fc >=> ns                                                                       ) cursor `shouldBe` Just 43
      ss <$> (fc >=> ns >=> fc                                                                ) cursor `shouldBe` Just 1
      ss <$> (fc >=> ns >=> fc >=> ns                                                         ) cursor `shouldBe` Just 1
      ss <$> (fc >=> ns >=> fc >=> ns >=> ns                                                  ) cursor `shouldBe` Just 1
      ss <$> (fc >=> ns >=> fc >=> ns >=> ns >=> ns                                           ) cursor `shouldBe` Just 9
      ss <$> (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                                    ) cursor `shouldBe` Just 1
      ss <$> (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns                             ) cursor `shouldBe` Just 1
      ss <$> (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns                      ) cursor `shouldBe` Just 1
      ss <$> (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns               ) cursor `shouldBe` Just 1
      ss <$> (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns        ) cursor `shouldBe` Just 1
      ss <$> (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns >=> ns ) cursor `shouldBe` Just 1
    it "can get token at cursor" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/data/sample.json" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString t u
      (jsonTokenAt                                                                                    ) cursor `shouldBe` Just (JsonTokenBraceL                             )
      (fc                                                                              >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenString "widget"                    )
      (fc >=> ns                                                                       >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenBraceL                             )
      (fc >=> ns >=> fc                                                                >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenString "debug"                     )
      (fc >=> ns >=> fc >=> ns                                                         >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenString "on"                        )
      (fc >=> ns >=> fc >=> ns >=> ns                                                  >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenString "window"                    )
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns                                           >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenBraceL                             )
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                                    >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenString "title"                     )
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns                             >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenString "Sample Konfabulator Widget")
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns                      >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenString "name"                      )
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns               >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenString "main_window"               )
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns        >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenString "width"                     )
      (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns >=> ns >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenNumber 500.0                       )
