{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Count
  ( cmdCount
  ) where

import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import Data.Semigroup                                 ((<>))
import Data.Word
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Json.LightJson
import HaskellWorks.Data.Json.Query
import HaskellWorks.Data.Json.Standard.Cursor.Generic
import HaskellWorks.Data.MQuery
import HaskellWorks.Data.MQuery.Micro
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.TreeCursor
import Options.Applicative                            hiding (columns)

import qualified App.Commands.Types                        as Z
import qualified Data.ByteString                           as BS
import qualified Data.ByteString.Internal                  as BSI
import qualified Data.DList                                as DL
import qualified Data.Vector.Storable                      as DVS
import qualified HaskellWorks.Data.BalancedParens.RangeMin as RM
import qualified System.IO.MMap                            as IO

siblings :: GenericCursor BSI.ByteString CsPoppy (RM.RangeMin CsPoppy) -> [GenericCursor BSI.ByteString CsPoppy (RM.RangeMin CsPoppy)]
siblings c = c:cs
  where cs = case nextSibling c of
          Just d  -> siblings d
          Nothing -> []

runCount :: Z.CountOptions -> IO ()
runCount opts = do
  let inputFile   = opts ^. the @"inputFile"
  let ibIndex     = opts ^. the @"ibIndex"
  let bpIndex     = opts ^. the @"bpIndex"
  let expression  = opts ^. the @"expression"
  jsonFr    <- IO.mmapFileForeignPtr inputFile IO.ReadOnly Nothing
  jsonIbFr  <- IO.mmapFileForeignPtr ibIndex   IO.ReadOnly Nothing
  jsonBpFr  <- IO.mmapFileForeignPtr bpIndex   IO.ReadOnly Nothing
  let jsonBs  = fromForeignRegion jsonFr    :: BS.ByteString
  let jsonIb  = fromForeignRegion jsonIbFr  :: DVS.Vector Word64
  let jsonBp  = fromForeignRegion jsonBpFr  :: DVS.Vector Word64

  let cursor = GenericCursor jsonBs (makeCsPoppy jsonIb) (RM.mkRangeMin (makeCsPoppy jsonBp)) 1

  let q = MQuery (DL.fromList $ fmap lightJsonAt (siblings cursor))

  putPretty $ q >>= (entry >=> named expression) & count

  return ()

optsCount :: Parser Z.CountOptions
optsCount = Z.CountOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input JSON file"
        <>  metavar "FILE"
        )
  <*> strOption
        (   long "ib-index"
        <>  help "IB index"
        <>  metavar "FILE"
        )
  <*> strOption
        (   long "bp-index"
        <>  help "BP index"
        <>  metavar "FILE"
        )
  <*> option auto
        (   long "expression"
        <>  help "JSON expression"
        <>  metavar "EXPRESSION"
        )

cmdCount :: Mod CommandFields (IO ())
cmdCount = command "count"  $ flip info idm $ runCount <$> optsCount
