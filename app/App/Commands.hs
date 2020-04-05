module App.Commands where

import App.Commands.Count
import App.Commands.CountAeson
import App.Commands.CreateIndex
import App.Commands.Demo
import Options.Applicative

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdCreateIndex
  <>  cmdCount
  <>  cmdCountAeson
  <>  cmdDemo
