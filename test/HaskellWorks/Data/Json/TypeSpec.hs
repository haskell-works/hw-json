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
import HaskellWorks.Data.BalancedParens.BalancedParens
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Json.Cursor
import HaskellWorks.Data.Json.Type
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString                              as BS
import qualified HaskellWorks.Data.Json.Backend.Standard.Fast as FAST
import qualified HaskellWorks.Data.Json.Backend.Standard.Slow as SLOW
import qualified HaskellWorks.Data.TreeCursor                 as TC

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

fc = TC.firstChild
ns = TC.nextSibling

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.CursorSpec" $ do
  genSpec "DVS.Vector Word64" SLOW.makeCursor
  genSpec "Poppy512"          FAST.makeCursor

genSpec :: forall t u.
  ( Eq                t
  , Show              t
  , Select1           t
  , Eq                u
  , Show              u
  , Rank0             u
  , Rank1             u
  , BalancedParens    u
  , TestBit           u)
  => String -> (String -> JsonCursor BS.ByteString t u) -> SpecWith ()
genSpec t makeCursor = do
  describe ("Json cursor of type " ++ t) $ do
    let forJson s f = describe ("of value " ++ show s) (f (makeCursor s))
    forJson "{}" $ \cursor -> do
      it "should have correct type"       $ requireTest $         jsonTypeAt  cursor === Just JsonTypeObject
    forJson " {}" $ \cursor -> do
      it "should have correct type"       $ requireTest $         jsonTypeAt  cursor === Just JsonTypeObject
    forJson "1234" $ \cursor -> do
      it "should have correct type"       $ requireTest $         jsonTypeAt  cursor === Just JsonTypeNumber
    forJson "\"Hello\"" $ \cursor -> do
      it "should have correct type"       $ requireTest $         jsonTypeAt  cursor === Just JsonTypeString
    forJson "[]" $ \cursor -> do
      it "should have correct type"       $ requireTest $         jsonTypeAt  cursor === Just JsonTypeArray
    forJson "true" $ \cursor -> do
      it "should have correct type"       $ requireTest $         jsonTypeAt  cursor === Just JsonTypeBool
    forJson "false" $ \cursor -> do
      it "should have correct type"       $ requireTest $         jsonTypeAt  cursor === Just JsonTypeBool
    forJson "null" $ \cursor -> do
      it "should have correct type"       $ requireTest $         jsonTypeAt  cursor === Just JsonTypeNull
    forJson "[null]" $ \cursor -> do
      it "should have correct type"       $ requireTest $ (fc >=> jsonTypeAt) cursor === Just JsonTypeNull
    forJson "[null, {\"field\": 1}]" $ \cursor -> do
      it "cursor can navigate to second child of array" $ requireTest $ do
        (fc >=> ns >=> jsonTypeAt) cursor === Just JsonTypeObject
      it "cursor can navigate to first child of object at second child of array" $ requireTest $ do
        (fc >=> ns >=> fc >=> jsonTypeAt) cursor === Just JsonTypeString
      it "cursor can navigate to first child of object at second child of array" $ requireTest $ do
        (fc >=> ns >=> fc >=> ns >=> jsonTypeAt) cursor === Just JsonTypeNumber
    describe "For empty json array" $ do
      let cursor = makeCursor "[null]"
      it "can navigate down and forwards" $ requireTest $ do
        (                     jsonTypeAt) cursor === Just JsonTypeArray
        (fc               >=> jsonTypeAt) cursor === Just JsonTypeNull
        (fc >=> ns        >=> jsonTypeAt) cursor === Nothing
        (fc >=> ns >=> ns >=> jsonTypeAt) cursor === Nothing
    describe "For sample Json" $ do
      let cursor = makeCursor "{ \
                    \    \"widget\": { \
                    \        \"debug\": \"on\", \
                    \        \"window\": { \
                    \            \"name\": \"main_window\", \
                    \            \"dimensions\": [500, 600.01e-02, true, false, null] \
                    \        } \
                    \    } \
                    \}" :: JsonCursor BS.ByteString t u
      it "can navigate down and forwards" $ requireTest $ do
        (                                                                      jsonTypeAt) cursor === Just JsonTypeObject
        (fc                                                                >=> jsonTypeAt) cursor === Just JsonTypeString
        (fc >=> ns                                                         >=> jsonTypeAt) cursor === Just JsonTypeObject
        (fc >=> ns >=> fc                                                  >=> jsonTypeAt) cursor === Just JsonTypeString
        (fc >=> ns >=> fc >=> ns                                           >=> jsonTypeAt) cursor === Just JsonTypeString
        (fc >=> ns >=> fc >=> ns >=> ns                                    >=> jsonTypeAt) cursor === Just JsonTypeString
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> jsonTypeAt) cursor === Just JsonTypeObject
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> jsonTypeAt) cursor === Just JsonTypeString
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> jsonTypeAt) cursor === Just JsonTypeString
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> jsonTypeAt) cursor === Just JsonTypeString
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> jsonTypeAt) cursor === Just JsonTypeArray
