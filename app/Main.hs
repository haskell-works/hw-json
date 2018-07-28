module Main where

import App.Commands
import Control.Monad
import Data.Semigroup      ((<>))
import Options.Applicative

main :: IO ()
main = join $ customExecParser
  (prefs $ showHelpOnEmpty <> showHelpOnError)
  (info (commands <**> helper) idm)
