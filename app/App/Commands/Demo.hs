{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Demo
  ( cmdDemo
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.ST
import Data.Generics.Product.Any
import Data.Semigroup                                     ((<>))
import Data.Word
import Foreign.ForeignPtr
import HaskellWorks.Data.Json.LightJson
import HaskellWorks.Data.Json.Query
import HaskellWorks.Data.Json.Standard.Cursor.Generic
import HaskellWorks.Data.Json.Standard.Cursor.Load.Cursor
import HaskellWorks.Data.MQuery
import HaskellWorks.Data.MQuery.Micro
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Vector.AsVector8
import Options.Applicative                                hiding (columns)

import qualified App.Commands.Types                         as Z
import qualified Data.ByteString                            as BS
import qualified Data.ByteString.Internal                   as BSI
import qualified Data.ByteString.Lazy                       as LBS
import qualified Data.DList                                 as DL
import qualified Data.Vector.Storable                       as DVS
import qualified Data.Vector.Storable.Mutable               as DVSM
import qualified HaskellWorks.Data.BalancedParens.RangeMin  as RM
import qualified HaskellWorks.Data.ByteString.Lazy          as LBS
import qualified HaskellWorks.Data.Json.Simd.Index.Standard as S
import qualified Options.Applicative                        as OA
import qualified System.IO                                  as IO
import qualified System.IO.MMap                             as IO

constructUnzipN :: Int -> [(BS.ByteString, BS.ByteString)] -> (DVS.Vector Word64, DVS.Vector Word64)
constructUnzipN nBytes xs = (DVS.unsafeCast ibv, DVS.unsafeCast bpv)
  where [ibv, bpv] = DVS.createT $ do
          let nW64s     = (nBytes + 7) `div` 8
          let capacity  = nW64s * 8
          ibmv <- DVSM.new capacity
          bpmv <- DVSM.new capacity
          (ibmvRemaining, bpmvRemaining) <- go ibmv bpmv xs
          return
            [ DVSM.take (((DVSM.length ibmv - ibmvRemaining) `div` 8) * 8) ibmv
            , DVSM.take (((DVSM.length bpmv - bpmvRemaining) `div` 8) * 8) bpmv
            ]
        go :: DVSM.MVector s Word8 -> DVSM.MVector s Word8 -> [(BS.ByteString, BS.ByteString)] -> ST s (Int, Int)
        go ibmv bpmv ((ib, bp):ys) = do
          DVS.copy (DVSM.take (BS.length ib) ibmv) (asVector8 ib)
          DVS.copy (DVSM.take (BS.length bp) bpmv) (asVector8 bp)
          go (DVSM.drop (BS.length ib) ibmv) (DVSM.drop (BS.length bp) bpmv) ys
        go ibmv bpmv [] = do
          DVSM.set (DVSM.take 8 ibmv) 0
          DVSM.set (DVSM.take 8 bpmv) 0
          return (DVSM.length (DVSM.drop 8 ibmv), DVSM.length (DVSM.drop 8 bpmv))

runDemo :: Z.DemoOptions -> IO ()
runDemo opts = do
  let filePath = opts ^. the @"filePath"
  case opts ^. the @"method" of
    "original" -> do
      !cursor <- loadCursor (opts ^. the @"filePath")
      let !json = lightJsonAt cursor
      let q = MQuery (DL.singleton json)

      putPretty $ q >>= (entry >=> named "meta" >=> entry >=> named "view" >=> entry >=> named "columns" >=> item >=> entry >=> named "id") & count
    "simd" -> do
      (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filePath IO.ReadOnly Nothing
      let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
      case S.makeStandardJsonIbBps (LBS.resegmentPadded 512 (LBS.fromStrict bs)) of
        Right ibBps -> do
          let (!ib, !bp) = constructUnzipN size ibBps
          let !cursor = GenericCursor bs (makeCsPoppy ib) (RM.mkRangeMin bp) 1
          let !json = lightJsonAt cursor
          let q = MQuery (DL.singleton json)

          putPretty $ q >>= (entry >=> named "meta" >=> entry >=> named "view" >=> entry >=> named "columns" >=> item >=> entry >=> named "id") & count
        Left msg -> IO.hPutStrLn IO.stderr $ "Unable to create semi-index: " <> show msg
    m -> IO.hPutStrLn IO.stderr $ "Unrecognised method: " <> show m

optsDemo :: Parser Z.DemoOptions
optsDemo = Z.DemoOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input DSV file"
        <>  metavar "STRING"
        )
  <*> strOption
        (   long "method"
        <>  short 'm'
        <>  help "Method (original|simd)"
        <>  metavar "STRING"
        <>  OA.value "original"
        )

cmdDemo :: Mod CommandFields (IO ())
cmdDemo = command "demo"  $ flip info idm $ runDemo <$> optsDemo
