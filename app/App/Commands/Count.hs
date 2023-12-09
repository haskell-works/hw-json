{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP #-}
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
import Data.Word
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Json.LightJson
import HaskellWorks.Data.Json.Query
import HaskellWorks.Data.Json.Standard.Cursor.Generic
import HaskellWorks.Data.MQuery
import HaskellWorks.Data.MQuery.Micro
import HaskellWorks.Data.RankSelect.CsPoppy1
import HaskellWorks.Data.TreeCursor
import Options.Applicative                            hiding (columns)

import qualified App.Commands.Types                          as Z
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Internal                    as BSI
import qualified Data.DList                                  as DL
import qualified Data.Vector.Storable                        as DVS
import qualified HaskellWorks.Data.BalancedParens.RangeMin   as RM
import qualified HaskellWorks.Data.Json.Standard.Cursor.Fast as JCF
import qualified System.IO.MMap                              as IO

siblings :: GenericCursor BSI.ByteString CsPoppy1 (RM.RangeMin CsPoppy1) -> [GenericCursor BSI.ByteString CsPoppy1 (RM.RangeMin CsPoppy1)]
siblings c = c:maybe [] siblings (nextSibling c)

runCount :: Z.CountOptions -> IO ()
#if aarch64_HOST_ARCH
runCount = undefined
#else
runCount opts = do
  let inputFile   = opts ^. the @"inputFile"
  let expression  = opts ^. the @"expression"
  jsonFr    <- IO.mmapFileForeignPtr inputFile IO.ReadOnly Nothing
  let jsonBs  = fromForeignRegion jsonFr    :: BS.ByteString

  cursor <- case opts ^. the @"indexes" of
    Just indexes -> do
      let ibIndex     = indexes ^. the @"ibIndex"
      let bpIndex     = indexes ^. the @"bpIndex"
      jsonIbFr  <- IO.mmapFileForeignPtr ibIndex   IO.ReadOnly Nothing
      jsonBpFr  <- IO.mmapFileForeignPtr bpIndex   IO.ReadOnly Nothing
      let jsonIb  = fromForeignRegion jsonIbFr  :: DVS.Vector Word64
      let jsonBp  = fromForeignRegion jsonBpFr  :: DVS.Vector Word64

      return $ GenericCursor jsonBs (makeCsPoppy jsonIb) (RM.mkRangeMin (makeCsPoppy jsonBp)) 1
    Nothing -> do
      let !ibip = JCF.simdToIbBp jsonBs
      let !c    = JCF.fromBsIbBp jsonBs ibip
      return c

  let q = MQuery (DL.fromList $ fmap lightJsonAt (siblings cursor))

  putPretty $ q >>= (entry >=> named expression) & count

  return ()
#endif

optsFileIndex :: Parser Z.FileIndexes
optsFileIndex = Z.FileIndexes
  <$> strOption
    (   long "ib-index"
    <>  help "IB index"
    <>  metavar "FILE"
    )
  <*> strOption
    (   long "bp-index"
    <>  help "BP index"
    <>  metavar "FILE"
    )

optsCount :: Parser Z.CountOptions
optsCount = Z.CountOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input JSON file"
        <>  metavar "FILE"
        )
  <*> optional optsFileIndex
  <*> strOption
        (   long "expression"
        <>  help "JSON expression"
        <>  metavar "EXPRESSION"
        )

cmdCount :: Mod CommandFields (IO ())
cmdCount = command "count"  $ flip info idm $ runCount <$> optsCount
