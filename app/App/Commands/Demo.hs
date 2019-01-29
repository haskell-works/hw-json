{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.Demo
  ( cmdDemo
  ) where

import App.Commands.Types
import Control.Lens
import Control.Monad
import Data.Semigroup                                      ((<>))
import HaskellWorks.Data.Json.Backend.Standard.Load.Cursor
import HaskellWorks.Data.Json.LightJson
import HaskellWorks.Data.Json.Query
import HaskellWorks.Data.MQuery
import HaskellWorks.Data.MQuery.Micro
import Options.Applicative                                 hiding (columns)

import qualified App.Lens   as L
import qualified Data.DList as DL

runDemo :: DemoOptions -> IO ()
runDemo opts = do
  !cursor <- loadCursor (opts ^. L.filePath)
  let !json = lightJsonAt cursor
  let q = MQuery (DL.singleton json)

  putPretty $ q >>= (entry >=> named "meta" >=> entry >=> named "view" >=> entry >=> named "columns" >=> item >=> entry >=> named "id") & count

optsDemo :: Parser DemoOptions
optsDemo = DemoOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input DSV file"
        <>  metavar "STRING"
        )

cmdDemo :: Mod CommandFields (IO ())
cmdDemo = command "demo"  $ flip info idm $ runDemo <$> optsDemo
