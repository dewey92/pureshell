module PureShell.Cat.Program where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (either)
import Effect.Class (liftEffect)
import Node.Path (FilePath)
import Options.Applicative (CommandFields, Mod, Parser, argument, command, info, metavar, progDesc, str)
import PureShell.AppM (AppM)
import PureShell.Cat.Cat (cat)
import PureShell.Common.Utility (logEscape)

program :: Mod CommandFields (AppM Unit)
program = command "cat" (info catParser $ progDesc "Simply read a file")

catParser :: Parser (AppM Unit)
catParser = ado
  filePath <- getFilePath
  in runCat filePath >>= logEscape >>> liftEffect
  where
    runCat fp = runExceptT (cat fp) <#> either show identity

getFilePath :: Parser FilePath
getFilePath = argument str (metavar "FILEPATH")
