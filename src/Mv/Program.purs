module PureShell.Mv.Program where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Options.Applicative (CommandFields, Mod, Parser, argument, command, help, info, metavar, progDesc, short, str, switch)
import PureShell.AppM (AppM)
import PureShell.Mv.Mv (mv)

program :: Mod CommandFields (AppM Unit)
program = command "mv" (info mvParser $ progDesc "move or rename files")

mvParser :: Parser (AppM Unit)
mvParser = ado
  (src /\ dest) <- arguments
  opts <- options
  in mv src dest

arguments :: Parser (String /\ String)
arguments = ado
  src <- argument str (metavar "SOURCE")
  dest <- argument str (metavar "DESTINATION")
  in (src /\ dest)

options :: Parser { withForce :: Boolean, withPrompt :: Boolean, withVerboseMessage :: Boolean }
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
