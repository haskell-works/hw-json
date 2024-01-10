{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.CountAeson
  ( cmdCountAeson
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Options.Applicative       hiding (columns)

import qualified App.Commands.Types   as Z
import qualified Data.Aeson           as J
import qualified Data.Aeson.Key       as J
import qualified Data.Aeson.KeyMap    as J
import qualified Data.ByteString.Lazy as LBS
import qualified System.IO            as IO

runCountAeson :: Z.CountAesonOptions -> IO ()
runCountAeson opts = do
  let inputFile   = opts ^. the @"inputFile"
  let expression  = opts ^. the @"expression"

  lbs <- LBS.readFile inputFile

  let lbsLines = LBS.split 10 lbs

  let count :: Int = sum $ flip fmap lbsLines $ \lbsLine -> case J.decode lbsLine of
        Just (J.Object v) -> if J.member (J.fromText expression) v then 1 else 0
        _                 -> 0

  IO.putStrLn $ "Count: " <> show count

  return ()

optsCountAeson :: Parser Z.CountAesonOptions
optsCountAeson = Z.CountAesonOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input JSON file"
        <>  metavar "FILE"
        )
  <*> strOption
        (   long "expression"
        <>  help "JSON expression"
        <>  metavar "EXPRESSION"
        )

cmdCountAeson :: Mod CommandFields (IO ())
cmdCountAeson = command "count-aeson"  $ flip info idm $ runCountAeson <$> optsCountAeson
