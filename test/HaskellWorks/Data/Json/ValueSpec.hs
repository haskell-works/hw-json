{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Json.ValueSpec (spec) where

import           Control.Monad
import qualified Data.ByteString                                            as BS
import           Data.String
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Decode
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Json.Succinct.Cursor                     as C
import           HaskellWorks.Data.Json.Succinct.Index
import           HaskellWorks.Data.Json.Value
import           HaskellWorks.Data.RankSelect.Base.Rank0
import           HaskellWorks.Data.RankSelect.Base.Rank1
import           HaskellWorks.Data.RankSelect.Base.Select1
import           HaskellWorks.Data.Succinct.BalancedParens.BalancedParens
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
import qualified HaskellWorks.Data.TreeCursor                               as TC
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

fc = TC.firstChild
ns = TC.nextSibling
-- cd = TC.depth

spec :: Spec
spec = describe "HaskellWorks.Data.Json.ValueSpec" $ do
  genSpec "DVS.Vector Word8"  (undefined :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8)))
  genSpec "DVS.Vector Word16" (undefined :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16)))
  genSpec "DVS.Vector Word32" (undefined :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32)))
  genSpec "DVS.Vector Word64" (undefined :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
  genSpec "Poppy512"          (undefined :: JsonCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64)))

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
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right (JsonObject [])
    forJson " {}" $ \cursor -> do
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right (JsonObject [])
    forJson "1234" $ \cursor -> do
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right (JsonNumber 1234)
    forJson "\"Hello\"" $ \cursor -> do
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right (JsonString "Hello")
    forJson "[]" $ \cursor -> do
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right (JsonArray [])
    forJson "true" $ \cursor -> do
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right (JsonBool True)
    forJson "false" $ \cursor -> do
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right (JsonBool False)
    forJson "null" $ \cursor -> do
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right JsonNull
    forJson "[null]" $ \cursor -> do
      it "should have correct value"      $ jsonValueVia (Just cursor) `shouldBe` Right (JsonArray [JsonNull])
      it "should have correct value"      $ jsonValueVia (fc   cursor) `shouldBe` Right  JsonNull
      -- it "depth at top"                   $ cd          cursor `shouldBe` Just 1
      -- it "depth at first child of array"  $ (fc >=> cd) cursor `shouldBe` Just 2
    forJson "[null, {\"field\": 1}]" $ \cursor -> do
      it "cursor can navigate to second child of array" $ do
        jsonValueVia ((fc >=> ns)   cursor) `shouldBe` Right (                     JsonObject [("field", JsonNumber 1)] )
        jsonValueVia (Just          cursor) `shouldBe` Right (JsonArray [JsonNull, JsonObject [("field", JsonNumber 1)]])
      -- it "depth at second child of array" $ do
      --   (fc >=> ns >=> cd) cursor `shouldBe` Just 2
      -- it "depth at first child of object at second child of array" $ do
      --   (fc >=> ns >=> fc >=> cd) cursor `shouldBe` Just 3
      -- it "depth at first child of object at second child of array" $ do
      --   (fc >=> ns >=> fc >=> ns >=> cd) cursor `shouldBe` Just 3
    describe "For empty json array" $ do
      let cursor =  "[]" :: JsonCursor BS.ByteString t u
      it "can navigate down and forwards" $ do
        jsonValueVia (Just cursor) `shouldBe` Right (JsonArray [])
    describe "For empty json array" $ do
      let cursor =  "[null]" :: JsonCursor BS.ByteString t u
      it "can navigate down and forwards" $ do
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
