module PureShell.Cat.Program where

import Prelude

import Effect.Class (liftEffect)
import Node.Path (FilePath)
import Options.Applicative (CommandFields, Mod, Parser, argument, command, info, metavar, progDesc, str)
import PureShell.AppM (AppM)
import PureShell.Cat.Cat (cat)
import PureShell.Common.Utility (logEscape)

program :: Mod CommandFields (AppM Unit)
program = command "cat" (info runCat $ progDesc "Simply read a file")

runCat :: Parser (AppM Unit)
runCat = ado
  opts <- options
  in cat opts >>= (logEscape >>> liftEffect)

options :: Parser FilePath
options = argument str (metavar "FILEPATH")
