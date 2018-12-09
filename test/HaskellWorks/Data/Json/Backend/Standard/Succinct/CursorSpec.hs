{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Json.Backend.Standard.Succinct.CursorSpec(spec) where

import Data.String
import Data.Word
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Json.Backend.Standard.Succinct.GenCursorTest
import HaskellWorks.Data.Json.Cursor
import HaskellWorks.Data.RankSelect.Poppy512
import Test.Hspec

import qualified Data.ByteString                              as BS
import qualified Data.Vector.Storable                         as DVS
import qualified HaskellWorks.Data.Json.Backend.Standard.Fast as FAST
import qualified HaskellWorks.Data.Json.Backend.Standard.Slow as SLOW

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.CursorSpec" $ do
  genTest "DVS.Vector Word64" (SLOW.makeCursor :: String -> JsonCursor BS.ByteString (DVS.Vector Word64) (SimpleBalancedParens (DVS.Vector Word64)))
  genTest "Poppy512"          (FAST.makeCursor :: String -> JsonCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64)))
