{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Json.TypeSpec (spec) where

import Control.Monad
import Data.Proxy
import Data.String
import Data.Word
import HaskellWorks.Data.BalancedParens.BalancedParens
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Json.Cursor
import HaskellWorks.Data.Json.Type
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.Poppy512
import Test.Hspec

import qualified Data.ByteString              as BS
import qualified Data.Vector.Storable         as DVS
import qualified HaskellWorks.Data.TreeCursor as TC

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

fc = TC.firstChild
ns = TC.nextSibling

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.CursorSpec" $ do
  genSpec "DVS.Vector Word64" (Proxy :: Proxy (JsonCursor BS.ByteString (DVS.Vector Word64) (SimpleBalancedParens (DVS.Vector Word64))))
  genSpec "Poppy512"          (Proxy :: Proxy (JsonCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64))))

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
  , IsString          (JsonCursor BS.ByteString t u))
  => String -> Proxy (JsonCursor BS.ByteString t u) -> SpecWith ()
genSpec t _ = do
  describe ("Json cursor of type " ++ t) $ do
    let forJson (cursor :: JsonCursor BS.ByteString t u) f = describe ("of value " ++ show cursor) (f cursor)
    forJson "{}" $ \cursor -> do
      it "should have correct type"       $         jsonTypeAt  cursor `shouldBe` Just JsonTypeObject
    forJson " {}" $ \cursor -> do
      it "should have correct type"       $         jsonTypeAt  cursor `shouldBe` Just JsonTypeObject
    forJson "1234" $ \cursor -> do
      it "should have correct type"       $         jsonTypeAt  cursor `shouldBe` Just JsonTypeNumber
    forJson "\"Hello\"" $ \cursor -> do
      it "should have correct type"       $         jsonTypeAt  cursor `shouldBe` Just JsonTypeString
    forJson "[]" $ \cursor -> do
      it "should have correct type"       $         jsonTypeAt  cursor `shouldBe` Just JsonTypeArray
    forJson "true" $ \cursor -> do
      it "should have correct type"       $         jsonTypeAt  cursor `shouldBe` Just JsonTypeBool
    forJson "false" $ \cursor -> do
      it "should have correct type"       $         jsonTypeAt  cursor `shouldBe` Just JsonTypeBool
    forJson "null" $ \cursor -> do
      it "should have correct type"       $         jsonTypeAt  cursor `shouldBe` Just JsonTypeNull
    forJson "[null]" $ \cursor -> do
      it "should have correct type"       $ (fc >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeNull
    forJson "[null, {\"field\": 1}]" $ \cursor -> do
      it "cursor can navigate to second child of array" $ do
        (fc >=> ns >=> jsonTypeAt)  cursor  `shouldBe` Just JsonTypeObject
      it "cursor can navigate to first child of object at second child of array" $ do
        (fc >=> ns >=> fc >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeString
      it "cursor can navigate to first child of object at second child of array" $ do
        (fc >=> ns >=> fc >=> ns >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeNumber
    describe "For empty json array" $ do
      let cursor =  "[null]" :: JsonCursor BS.ByteString t u
      it "can navigate down and forwards" $ do
        (                     jsonTypeAt) cursor `shouldBe` Just JsonTypeArray
        (fc               >=> jsonTypeAt) cursor `shouldBe` Just JsonTypeNull
        (fc >=> ns        >=> jsonTypeAt) cursor `shouldBe` Nothing
        (fc >=> ns >=> ns >=> jsonTypeAt) cursor `shouldBe` Nothing
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
