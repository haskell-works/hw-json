{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures -dshow-passes -fno-spec-constr #-}

module HaskellWorks.Data.Json.Backend.Standard.Succinct.CursorSpec(spec) where

import Control.Monad
import Data.String
import Data.Word
import HaskellWorks.Data.BalancedParens.BalancedParens
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Json.Cursor
import HaskellWorks.Data.Json.Internal.Backend.Standard.Cursor.Token
import HaskellWorks.Data.Json.Internal.Index
import HaskellWorks.Data.Json.Internal.Token
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
pn = TC.parent
ss = TC.subtreeSize

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.CursorSpec" $ do
  genSpec "DVS.Vector Word64" (undefined :: JsonCursor BS.ByteString (DVS.Vector Word64) (SimpleBalancedParens (DVS.Vector Word64)))
  genSpec "Poppy512"          (undefined :: JsonCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64)))

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
  => String -> (JsonCursor BS.ByteString t u) -> SpecWith ()
genSpec t _ = do
  describe ("Json cursor of type " ++ t) $ do
    describe "For empty json array" $ do
      let cursor =  "[null]" :: JsonCursor BS.ByteString t u
      it "can navigate down and forwards" $ do
        jsonIndexAt cursor `shouldBe` Right (JsonIndexArray [JsonIndexNull])
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
