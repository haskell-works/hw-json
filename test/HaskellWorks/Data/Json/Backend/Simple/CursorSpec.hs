{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Json.Backend.Simple.CursorSpec
  ( spec
  ) where

import Control.Monad
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString                              as BS
import qualified HaskellWorks.Data.Json.Backend.Simple.Cursor as Z
import qualified HaskellWorks.Data.Json.Backend.Simple.Fast   as FAST
import qualified HaskellWorks.Data.Json.Backend.Simple.Value  as V
import qualified HaskellWorks.Data.TreeCursor                 as TC

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

fc = TC.firstChild
ns = TC.nextSibling

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Backend.Simple.CursorSpec" $ do
  describe "Json cursor" $ do
    describe "For sample Json" $ do
      let k = FAST.makeCursor ("[[1],[2]]" :: BS.ByteString)
      -- [  [  1  ]  ,  [  2  ]  ]
      -- (( ((    )) )( ((    )) ))
      it "can navigate" $ requireTest $ do
        (Z.cursorRank <$>  Just                            k) === Just 1
        (Z.cursorRank <$>  ns                              k) === Nothing
        (Z.cursorRank <$>  fc                              k) === Just 2
        (Z.cursorRank <$> (fc >=> ns                     ) k) === Just 8
        (Z.cursorRank <$> (fc >=> ns >=> fc              ) k) === Just 9
        (Z.cursorRank <$> (fc >=> ns >=> fc >=> ns       ) k) === Nothing
        (Z.cursorRank <$> (fc >=> ns >=> fc >=> fc       ) k) === Just 10
        (Z.cursorRank <$> (fc >=> ns >=> fc >=> fc >=> ns) k) === Nothing
        (Z.cursorRank <$> (fc >=> ns >=> fc >=> fc >=> fc) k) === Nothing
        (Z.cursorRank <$> (fc >=> fc                     ) k) === Just 3
        (Z.cursorRank <$> (fc >=> fc >=> ns              ) k) === Nothing
        (Z.cursorRank <$> (fc >=> fc >=> fc              ) k) === Just 4
        (Z.cursorRank <$> (fc >=> fc >=> fc >=> ns       ) k) === Nothing
        (Z.cursorRank <$> (fc >=> fc >=> fc >=> fc       ) k) === Nothing
      it "can snippet" $ requireTest $ do
        (V.snippet <$>  Just                            k) === Just "[[1],[2]]"
        (V.snippet <$>  ns                              k) === Nothing
        (V.snippet <$>  fc                              k) === Just "[1]"
        (V.snippet <$> (fc >=> ns                     ) k) === Just "[2]"
        (V.snippet <$> (fc >=> ns >=> fc              ) k) === Just "[2]"
        (V.snippet <$> (fc >=> ns >=> fc >=> ns       ) k) === Nothing
        (V.snippet <$> (fc >=> ns >=> fc >=> fc       ) k) === Just ""
        (V.snippet <$> (fc >=> ns >=> fc >=> fc >=> ns) k) === Nothing
        (V.snippet <$> (fc >=> ns >=> fc >=> fc >=> fc) k) === Nothing
        (V.snippet <$> (fc >=> fc                     ) k) === Just "[1]"
        (V.snippet <$> (fc >=> fc >=> ns              ) k) === Nothing
        (V.snippet <$> (fc >=> fc >=> fc              ) k) === Just ""
        (V.snippet <$> (fc >=> fc >=> fc >=> ns       ) k) === Nothing
        (V.snippet <$> (fc >=> fc >=> fc >=> fc       ) k) === Nothing
