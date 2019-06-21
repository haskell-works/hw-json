{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Json.Backend.Standard.Succinct.GenCursorTest(genTest) where

import Control.Monad
import HaskellWorks.Data.BalancedParens.BalancedParens
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Json.Backend.Standard.Cursor.Generic
import HaskellWorks.Data.Json.Internal.Backend.Standard.Cursor.Token
import HaskellWorks.Data.Json.Internal.Index
import HaskellWorks.Data.Json.Internal.Token
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString              as BS
import qualified HaskellWorks.Data.TreeCursor as TC

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

fc = TC.firstChild
ns = TC.nextSibling
pn = TC.parent
ss = TC.subtreeSize

genTest :: forall t u.
  ( Eq                t
  , Show              t
  , Select1           t
  , Eq                u
  , Show              u
  , Rank0             u
  , Rank1             u
  , BalancedParens    u
  , TestBit           u)
  => String -> (String -> GenericCursor BS.ByteString t u) -> SpecWith ()
genTest t mkCursor = do
  describe ("Json cursor of type " ++ t) $ do
    describe "For empty json array" $ do
      let cursor = mkCursor "[null]"
      it "can navigate down and forwards" $ requireTest $ do
        jsonIndexAt cursor === Right (JsonIndexArray [JsonIndexNull])
    describe "For sample Json" $ do
      let cursor = mkCursor "\
            \{ \
            \    \"widget\": { \
            \        \"debug\": \"on\", \
            \        \"window\": { \
            \            \"name\": \"main_window\", \
            \            \"dimensions\": [500, 600.01e-02, true, false, null] \
            \        } \
            \    } \
            \}"
      it "can navigate up" $ requireTest $ do
        (                                                                      pn) cursor === Nothing
        (fc                                                                >=> pn) cursor ===                                    Just cursor
        (fc >=> ns                                                         >=> pn) cursor ===                                    Just cursor
        (fc >=> ns >=> fc                                                  >=> pn) cursor === (fc >=> ns                            ) cursor
        (fc >=> ns >=> fc >=> ns                                           >=> pn) cursor === (fc >=> ns                            ) cursor
        (fc >=> ns >=> fc >=> ns >=> ns                                    >=> pn) cursor === (fc >=> ns                            ) cursor
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> pn) cursor === (fc >=> ns                            ) cursor
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> pn) cursor === (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> pn) cursor === (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> pn) cursor === (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> pn) cursor === (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
      it "can get subtree size" $ requireTest $ do
        (                                                                      ss) cursor === Just 16
        (fc                                                                >=> ss) cursor === Just 1
        (fc >=> ns                                                         >=> ss) cursor === Just 14
        (fc >=> ns >=> fc                                                  >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns                                           >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns >=> ns                                    >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> ss) cursor === Just 10
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ss) cursor === Just 6
      it "can get token at cursor" $ requireTest $ do
        (jsonTokenAt                                                                      ) cursor === Just (JsonTokenBraceL                 )
        (fc                                                                >=> jsonTokenAt) cursor === Just (JsonTokenString   "widget"      )
        (fc >=> ns                                                         >=> jsonTokenAt) cursor === Just (JsonTokenBraceL                 )
        (fc >=> ns >=> fc                                                  >=> jsonTokenAt) cursor === Just (JsonTokenString   "debug"       )
        (fc >=> ns >=> fc >=> ns                                           >=> jsonTokenAt) cursor === Just (JsonTokenString   "on"          )
        (fc >=> ns >=> fc >=> ns >=> ns                                    >=> jsonTokenAt) cursor === Just (JsonTokenString   "window"      )
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> jsonTokenAt) cursor === Just (JsonTokenBraceL                 )
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> jsonTokenAt) cursor === Just (JsonTokenString   "name"        )
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> jsonTokenAt) cursor === Just (JsonTokenString   "main_window" )
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> jsonTokenAt) cursor === Just (JsonTokenString   "dimensions"  )
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> jsonTokenAt) cursor === Just (JsonTokenBracketL               )
