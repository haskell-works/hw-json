{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.Demo
  ( cmdDemo
  ) where

import App.Commands.Types
import Control.Lens
import Control.Monad
import Data.Semigroup                          ((<>))
import Data.Word
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.Json.Cursor
import HaskellWorks.Data.Json.LightJson
import HaskellWorks.Data.Json.LoadCursor
import HaskellWorks.Data.Micro
import HaskellWorks.Data.MQuery
import Options.Applicative                     hiding (columns)

import qualified App.Lens             as L
import qualified Data.ByteString      as BS
import qualified Data.DList           as DL
import qualified Data.Vector.Storable as DVS

readJson :: String -> IO (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
readJson path = do
  bs <- BS.readFile path
  print ("Read file" :: String)
  let !cursor = fromByteString bs :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
  print ("Created cursor" :: String)
  return cursor

runDemo :: DemoOptions -> IO ()
runDemo opts = do
  !cursor <- loadJsonWithCsPoppyIndex (opts ^. L.filePath)
  let !json = lightJsonAt cursor
  let q = MQuery (DL.singleton json)

  putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code" >=> asString) & count

optsDemo :: Parser DemoOptions
optsDemo = DemoOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input DSV file"
        <>  metavar "STRING"
        )

cmdDemo :: Mod CommandFields (IO ())
cmdDemo = command "create-index"  $ flip info idm $ runDemo <$> optsDemo
