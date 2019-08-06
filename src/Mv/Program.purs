module PureShell.Mv.Program where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (either)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Options.Applicative (CommandFields, Mod, Parser, argument, command, help, info, metavar, progDesc, short, str, switch)
import PureShell.AppM (AppM)
import PureShell.Common.Utility (logEscape)
import PureShell.Mv.Mv (MvOptions, mv)

program :: Mod CommandFields (AppM Unit)
program = command "mv" (info mvParser $ progDesc "move or rename files")

mvParser :: Parser (AppM Unit)
mvParser = ado
  (src /\ dest) <- arguments
  opts <- options
  in (runExceptT (mv src dest opts) <#> either show identity) >>= logEscape >>> liftEffect

arguments :: Parser (String /\ String)
arguments = ado
  src <- argument str (metavar "SOURCE")
  dest <- argument str (metavar "DESTINATION")
  in (src /\ dest)

options :: Parser MvOptions
options = ado
  withForce <- switch (
    short 'f' <>
    help "force"
  )
  withPrompt <- switch (
    short 'i' <>
    help "show prompt before executing"
  )
  withVerboseMessage <- switch (
    short 'v' <>
    help "verbose"
  )
  in { withForce, withPrompt, withVerboseMessage }
