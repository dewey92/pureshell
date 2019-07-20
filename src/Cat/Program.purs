module PureShell.Cat.Program where

import Prelude

import Effect.Aff (Aff)
import Node.Path (FilePath)
import Options.Applicative (CommandFields, Mod, Parser, argument, command, info, metavar, progDesc, str)
import PureShell.Cat.Cat (cat)

program :: Mod CommandFields (Aff Unit)
program = command "cat" (info (cat <$> options) $ progDesc "Simply read a file")

options :: Parser FilePath
options = argument str (metavar "FILEPATH")
