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
import           Data.String
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShow
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Decode
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Json.Succinct.Cursor                     as C
import           HaskellWorks.Data.Json.Succinct.Index
import           HaskellWorks.Data.Json.Token
import           HaskellWorks.Data.Json.Type
import           HaskellWorks.Data.Json.Value
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
import qualified HaskellWorks.Data.TreeCursor as TC
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

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
      jsonTypeAt cursor `shouldBe` Just JsonTypeObject
    it "initialises to beginning of empty object preceded by spaces" $ do
      let cursor = " {}" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonTypeAt cursor `shouldBe` Just JsonTypeObject
    it "initialises to beginning of number" $ do
      let cursor = "1234" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonTypeAt cursor `shouldBe` Just JsonTypeNumber
    it "initialises to beginning of string" $ do
      let cursor = "\"Hello\"" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonTypeAt cursor `shouldBe` Just JsonTypeString
    it "initialises to beginning of array" $ do
      let cursor = "[]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonTypeAt cursor `shouldBe` Just JsonTypeArray
    it "initialises to beginning of boolean true" $ do
      let cursor = "true" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonTypeAt cursor `shouldBe` Just JsonTypeBool
    it "initialises to beginning of boolean false" $ do
      let cursor = "false" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonTypeAt cursor `shouldBe` Just JsonTypeBool
    it "initialises to beginning of null" $ do
      let cursor = "null" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonTypeAt cursor `shouldBe` Just JsonTypeNull
    it "cursor can navigate to first child of array" $ do
      let cursor = "[null]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeNull
    it "cursor can navigate to second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> ns >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeObject
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> ns >=> fc >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeString
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> ns >=> fc >=> ns >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeNumber
    it "depth at top" $ do
      let cursor = "[null]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd cursor `shouldBe` Just 1
    it "depth at first child of array" $ do
      let cursor = "[null]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> cd) cursor `shouldBe` Just 2
    it "depth at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> ns >=> cd) cursor `shouldBe` Just 2
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> ns >=> fc >=> cd) cursor `shouldBe` Just 3
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> ns >=> fc >=> ns >=> cd) cursor `shouldBe` Just 3
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

jsonValueVia  :: JsonIndexAt (JsonCursor BS.ByteString t u)
              => Maybe (JsonCursor BS.ByteString t u) -> Either DecodeError JsonValue
jsonValueVia mk = case mk of
  Just k    -> (jsonIndexAt >=> jsonValueAt) k
  Nothing   -> Left (DecodeError "No such element")

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
  , JsonIndexAt       (JsonCursor BS.ByteString t u)
  )
  => String -> (JsonCursor BS.ByteString t u) -> SpecWith ()
genSpec t _ = do
  describe ("Json cursor of type " ++ t) $ do
    let forJson (cursor :: JsonCursor BS.ByteString t u) f = describe ("of value " ++ show cursor) (f cursor)
    forJson "{}" $ \cursor -> do
      it "should have correct type"       $ jsonTypeAt         cursor  `shouldBe` Just   JsonTypeObject
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right (JsonObject [])
    forJson " {}" $ \cursor -> do
      it "should have correct type"       $ jsonTypeAt         cursor  `shouldBe` Just   JsonTypeObject
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right (JsonObject [])
    forJson "1234" $ \cursor -> do
      it "should have correct type"       $ jsonTypeAt         cursor  `shouldBe` Just   JsonTypeNumber
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right (JsonNumber 1234)
    forJson "\"Hello\"" $ \cursor -> do
      it "should have correct type"       $ jsonTypeAt         cursor  `shouldBe` Just   JsonTypeString
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right (JsonString "Hello")
    forJson "[]" $ \cursor -> do
      it "should have correct type"       $ jsonTypeAt         cursor  `shouldBe` Just   JsonTypeArray
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right (JsonArray [])
    forJson "true" $ \cursor -> do
      it "should have correct type"       $ jsonTypeAt         cursor  `shouldBe` Just   JsonTypeBool
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right (JsonBool True)
    forJson "false" $ \cursor -> do
      it "should have correct type"       $ jsonTypeAt         cursor  `shouldBe` Just   JsonTypeBool
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right (JsonBool False)
    forJson "null" $ \cursor -> do
      it "should have correct type"       $ jsonTypeAt         cursor  `shouldBe` Just  JsonTypeNull
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right JsonNull
    forJson "[null]" $ \cursor -> do
      it "should have correct type"       $ (fc >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeNull
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right (JsonArray [JsonNull])
      it "should have correct value"      $ jsonValueVia (fc   cursor) `shouldBe` Right  JsonNull
      it "depth at top"                   $ cd          cursor `shouldBe` Just 1
      it "depth at first child of array"  $ (fc >=> cd) cursor `shouldBe` Just 2
    forJson "[null, {\"field\": 1}]" $ \cursor -> do
      it "cursor can navigate to second child of array" $ do
        (fc >=> ns >=> jsonTypeAt)  cursor  `shouldBe` Just JsonTypeObject
        jsonValueVia ((fc >=> ns)   cursor) `shouldBe` Right (                     JsonObject [("field", JsonNumber 1)] )
        jsonValueVia (Just          cursor) `shouldBe` Right (JsonArray [JsonNull, JsonObject [("field", JsonNumber 1)]])
      it "cursor can navigate to first child of object at second child of array" $ do
        (fc >=> ns >=> fc >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeString
      it "cursor can navigate to first child of object at second child of array" $ do
        (fc >=> ns >=> fc >=> ns >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeNumber
      it "depth at second child of array" $ do
        (fc >=> ns >=> cd) cursor `shouldBe` Just 2
      it "depth at first child of object at second child of array" $ do
        (fc >=> ns >=> fc >=> cd) cursor `shouldBe` Just 3
      it "depth at first child of object at second child of array" $ do
        (fc >=> ns >=> fc >=> ns >=> cd) cursor `shouldBe` Just 3
    describe "For empty json array" $ do
      let cursor =  "[]" :: JsonCursor BS.ByteString t u
      it "can navigate down and forwards" $ do
        jsonValueVia (Just cursor) `shouldBe` Right (JsonArray [])
    describe "For empty json array" $ do
      let cursor =  "[null]" :: JsonCursor BS.ByteString t u
      it "can navigate down and forwards" $ do
        (                     jsonTypeAt) cursor `shouldBe` Just JsonTypeArray
        (fc               >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeNull
        (fc >=> ns        >=> jsonTypeAt) cursor `shouldBe` Nothing
        (fc >=> ns >=> ns >=> jsonTypeAt) cursor `shouldBe` Nothing
      it "can navigate down and forwards" $ do
        jsonIndexAt cursor `shouldBe` Right (JsonIndexArray [JsonIndexNull])
        jsonValueVia (Just cursor) `shouldBe` Right (JsonArray [JsonNull])
    describe "For sample Json" $ do
      let cursor =  "{ \
                    \    \"widget\": { \
                    \        \"debug\": \"on\", \
                    \        \"window\": { \
                    \            \"name\": \"main_window\", \
                    \            \"dimensions\": [500, 600.01e-02, true, false, null] \
                    \        } \
                    \    } \
                    \}" :: JsonCursor BS.ByteString t u
      it "can navigate down and forwards" $ do
        let array   = JsonArray [JsonNumber 500, JsonNumber 600.01e-02, JsonBool True, JsonBool False, JsonNull] :: JsonValue
        let object1 = JsonObject ([("name", JsonString "main_window"), ("dimensions", array)]) :: JsonValue
        let object2 = JsonObject ([("debug", JsonString "on"), ("window", object1)]) :: JsonValue
        let object3 = JsonObject ([("widget", object2)]) :: JsonValue
        jsonValueVia (Just                                                                                                   cursor) `shouldBe` Right object3
        jsonValueVia ((fc                                                                                                  ) cursor) `shouldBe` Right (JsonString "widget"      )
        jsonValueVia ((fc >=> ns                                                                                           ) cursor) `shouldBe` Right (object2                  )
        jsonValueVia ((fc >=> ns >=> fc                                                                                    ) cursor) `shouldBe` Right (JsonString "debug"       )
        jsonValueVia ((fc >=> ns >=> fc >=> ns                                                                             ) cursor) `shouldBe` Right (JsonString "on"          )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns                                                                      ) cursor) `shouldBe` Right (JsonString "window"      )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns                                                               ) cursor) `shouldBe` Right (object1                  )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                                                        ) cursor) `shouldBe` Right (JsonString "name"        )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns                                                 ) cursor) `shouldBe` Right (JsonString "main_window" )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns                                          ) cursor) `shouldBe` Right (JsonString "dimensions"  )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns                                   ) cursor) `shouldBe` Right (array                    )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                            ) cursor) `shouldBe` Right (JsonNumber 500           )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns                     ) cursor) `shouldBe` Right (JsonNumber 600.01e-02    )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns              ) cursor) `shouldBe` Right (JsonBool True            )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns       ) cursor) `shouldBe` Right (JsonBool False           )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns) cursor) `shouldBe` Right JsonNull
      it "can navigate down and forwards" $ do
        (                                                                      jsonTypeAt) cursor `shouldBe` Just JsonTypeObject
        (fc                                                                >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeString
        (fc >=> ns                                                         >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeObject
        (fc >=> ns >=> fc                                                  >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeString
        (fc >=> ns >=> fc >=> ns                                           >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeString
        (fc >=> ns >=> fc >=> ns >=> ns                                    >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeString
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeObject
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeString
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeString
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeString
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeArray
      it "can navigate up" $ do
        (                                                                      pn) cursor `shouldBe` Nothing
        (fc                                                                >=> pn) cursor `shouldBe`                                    Just cursor
        (fc >=> ns                                                         >=> pn) cursor `shouldBe`                                    Just cursor
        (fc >=> ns >=> fc                                                  >=> pn) cursor `shouldBe` (fc >=> ns                            ) cursor
        (fc >=> ns >=> fc >=> ns                                           >=> pn) cursor `shouldBe` (fc >=> ns                            ) cursor
        (fc >=> ns >=> fc >=> ns >=> ns                                    >=> pn) cursor `shouldBe` (fc >=> ns                            ) cursor
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> pn) cursor `shouldBe` (fc >=> ns                            ) cursor
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> pn) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> pn) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> pn) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> pn) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
      it "can get subtree size" $ do
        (                                                                      ss) cursor `shouldBe` Just 16
        (fc                                                                >=> ss) cursor `shouldBe` Just 1
        (fc >=> ns                                                         >=> ss) cursor `shouldBe` Just 14
        (fc >=> ns >=> fc                                                  >=> ss) cursor `shouldBe` Just 1
        (fc >=> ns >=> fc >=> ns                                           >=> ss) cursor `shouldBe` Just 1
        (fc >=> ns >=> fc >=> ns >=> ns                                    >=> ss) cursor `shouldBe` Just 1
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> ss) cursor `shouldBe` Just 10
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> ss) cursor `shouldBe` Just 1
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> ss) cursor `shouldBe` Just 1
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> ss) cursor `shouldBe` Just 1
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ss) cursor `shouldBe` Just 6
      it "can get token at cursor" $ do
        (jsonTokenAt                                                                      ) cursor `shouldBe` Just (JsonTokenBraceL                 )
        (fc                                                                >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenString   "widget"      )
        (fc >=> ns                                                         >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenBraceL                 )
        (fc >=> ns >=> fc                                                  >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenString   "debug"       )
        (fc >=> ns >=> fc >=> ns                                           >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenString   "on"          )
        (fc >=> ns >=> fc >=> ns >=> ns                                    >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenString   "window"      )
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenBraceL                 )
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenString   "name"        )
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenString   "main_window" )
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenString   "dimensions"  )
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> jsonTokenAt) cursor `shouldBe` Just (JsonTokenBracketL               )
