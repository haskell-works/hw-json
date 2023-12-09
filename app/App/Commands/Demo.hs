{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Demo
  ( cmdDemo
  ) where

import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import Data.Word
import Foreign.ForeignPtr
import HaskellWorks.Data.Json.LightJson
import HaskellWorks.Data.Json.Query
import HaskellWorks.Data.Json.Standard.Cursor.Generic
import HaskellWorks.Data.Json.Standard.Cursor.Load.Cursor
import HaskellWorks.Data.MQuery
import HaskellWorks.Data.MQuery.Micro
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Vector.Storable
import Options.Applicative                                hiding (columns)

import qualified App.Commands.Types                         as Z
import qualified Data.ByteString.Internal                   as BSI
import qualified Data.ByteString.Lazy                       as LBS
import qualified Data.DList                                 as DL
import qualified HaskellWorks.Data.BalancedParens.RangeMin  as RM
import qualified HaskellWorks.Data.ByteString.Lazy          as LBS
#if x86_64_HOST_ARCH
import qualified HaskellWorks.Data.Json.Simd.Index.Standard as S
#endif
import qualified Options.Applicative                        as OA
import qualified System.IO                                  as IO
import qualified System.IO.MMap                             as IO

{- HLINT ignore "Reduce duplication" -}

runDemo :: Z.DemoOptions -> IO ()
#if x86_64_HOST_ARCH
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
          let (!ib, !bp) = construct64UnzipN size ibBps
          let !cursor = GenericCursor bs (makeCsPoppy ib) (RM.mkRangeMin bp) 1
          let !json = lightJsonAt cursor
          let q = MQuery (DL.singleton json)

          putPretty $ q >>= (entry >=> named "meta" >=> entry >=> named "view" >=> entry >=> named "columns" >=> item >=> entry >=> named "id") & count
        Left msg -> IO.hPutStrLn IO.stderr $ "Unable to create semi-index: " <> show msg
    m -> IO.hPutStrLn IO.stderr $ "Unrecognised method: " <> show m
#else
runDemo opts = undefined
#endif

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
