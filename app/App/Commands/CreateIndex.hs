{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.CreateIndex
  ( cmdCreateIndex
  ) where

import App.Commands.Types
import Control.Lens
import Data.Semigroup      ((<>))
import Data.Word
import Foreign
import Options.Applicative hiding (columns)

import qualified App.Lens                                                            as L
import qualified Data.ByteString.Builder                                             as B
import qualified Data.ByteString.Internal                                            as BSI
import qualified Data.ByteString.Lazy                                                as LBS
import qualified Data.Vector.Storable                                                as DVS
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.Blank              as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.BlankedJson        as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.MakeIndex          as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.ToBalancedParens64 as J
import qualified System.IO                                                           as IO
import qualified System.IO.MMap                                                      as IO

runCreateIndex :: CreateIndexOptions -> IO ()
runCreateIndex opts = do
  let filePath = opts ^. L.filePath
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filePath IO.ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  let blankedJson = J.blankJson [bs]
  let ibs = LBS.fromChunks (J.blankedJsonToInterestBits blankedJson)
  let bps = J.toBalancedParens64 (J.BlankedJson blankedJson)
  let vb = DVS.foldl (\b a -> b <> B.word64LE a) mempty bps
  LBS.writeFile (filePath <> ".ib.idx") ibs
  h <- IO.openFile (filePath <> ".bp.idx") IO.WriteMode
  B.hPutBuilder h vb
  IO.hClose h

optsCreateIndex :: Parser CreateIndexOptions
optsCreateIndex = CreateIndexOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input JSON file"
        <>  metavar "STRING"
        )

cmdCreateIndex :: Mod CommandFields (IO ())
cmdCreateIndex = command "create-index"  $ flip info idm $ runCreateIndex <$> optsCreateIndex
