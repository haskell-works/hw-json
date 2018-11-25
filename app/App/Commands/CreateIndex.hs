{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.CreateIndex
  ( cmdCreateIndex
  ) where

import App.Commands.Types
import Control.Lens
import Data.Maybe
import Data.Semigroup      ((<>))
import Data.Word
import Foreign
import Options.Applicative hiding (columns)

import qualified App.Lens                                                            as L
import qualified Data.ByteString.Internal                                            as BSI
import qualified Data.ByteString.Lazy                                                as LBS
import qualified HaskellWorks.Data.ByteString.Lazy                                   as LBS
import qualified HaskellWorks.Data.Json.Backend.Simple.SemiIndex                     as SISI
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.Blank              as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.BlankedJson        as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.MakeIndex          as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.ToBalancedParens64 as J
import qualified System.IO                                                           as IO
import qualified System.IO.MMap                                                      as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

runCreateIndexStandard :: CreateIndexOptions -> IO ()
runCreateIndexStandard opts = do
  let filePath = opts ^. L.filePath
  let outputIbFile = opts ^. L.outputIbFile & fromMaybe (filePath <> ".ib.idx")
  let outputBpFile = opts ^. L.outputBpFile & fromMaybe (filePath <> ".bp.idx")
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filePath IO.ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  let blankedJson = J.blankJson [bs]
  let ibs = LBS.fromChunks (J.blankedJsonToInterestBits blankedJson)
  let bps = J.toBalancedParens64 (J.BlankedJson blankedJson)
  LBS.writeFile outputIbFile ibs
  LBS.writeFile outputBpFile (LBS.toLazyByteString bps)

runCreateIndexSimple :: CreateIndexOptions -> IO ()
runCreateIndexSimple opts = do
  let filePath = opts ^. L.filePath
  let outputIbFile = opts ^. L.outputIbFile & fromMaybe (filePath <> ".ib.idx")
  let outputBpFile = opts ^. L.outputBpFile & fromMaybe (filePath <> ".bp.idx")
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filePath IO.ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  let SISI.SemiIndex _ ibs bps = SISI.buildSemiIndex bs
  LBS.writeFile outputIbFile (LBS.toLazyByteString ibs)
  LBS.writeFile outputBpFile (LBS.toLazyByteString bps)

runCreateIndex :: CreateIndexOptions -> IO ()
runCreateIndex opts = case opts ^. L.backend of
  "standard" -> runCreateIndexStandard  opts
  "simple"   -> runCreateIndexSimple    opts
  unknown    -> IO.hPutStrLn IO.stderr $ "Unknown backend " <> show unknown

optsCreateIndex :: Parser CreateIndexOptions
optsCreateIndex = CreateIndexOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input JSON file"
        <>  metavar "STRING"
        )
  <*> strOption
        (   long "backend"
        <>  short 'm'
        <>  value "standard"
        <>  help "Backend for creating index"
        <>  metavar "STRING"
        )
  <*> optional
        ( strOption
          (   long "output-ib-file"
          <>  help "Filename for output ib index"
          <>  metavar "STRING"
          )
        )
  <*> optional
        ( strOption
          (   long "output-bp-file"
          <>  help "Filename for output bp index"
          <>  metavar "STRING"
          )
        )

cmdCreateIndex :: Mod CommandFields (IO ())
cmdCreateIndex = command "create-index"  $ flip info idm $ runCreateIndex <$> optsCreateIndex
