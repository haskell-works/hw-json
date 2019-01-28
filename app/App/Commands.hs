module App.Commands where

import App.Commands.CreateIndex
import App.Commands.Demo
import Data.Semigroup           ((<>))
import Options.Applicative

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdCreateIndex
  <>  cmdDemo
