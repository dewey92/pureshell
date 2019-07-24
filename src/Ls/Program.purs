module PureShell.Ls.Program where

import Prelude

import Data.List (List(..), intercalate, length, singleton, zipWith)
import Data.Traversable (traverse)
import Effect.Class (liftEffect)
import Node.Path (FilePath)
import Options.Applicative (CommandFields, Mod, Parser, argument, command, help, info, many, metavar, progDesc, short, str, switch)
import PureShell.AppM (AppM)
import PureShell.Common.Utility (logEscape)
import PureShell.Ls.Ls (ls)
import PureShell.Ls.Types (LsOptions)

program :: Mod CommandFields (AppM Unit)
program = command "ls" (info lsParser $ progDesc "List a directory")

-- | Combine all parsers and run the `ls` program. Since ls accepts multiple arguments,
-- | it should traverse over the arguments to produce side-effects
lsParser :: Parser (AppM Unit)
lsParser = ado
  fps <- filePathsToShow
  opts <- options
  in runMultipleFilePaths fps opts
  where
    runMultipleFilePaths :: List FilePath -> LsOptions -> AppM Unit
    runMultipleFilePaths fps opts = traverse (\fp -> ls fp opts) fps
      <#> zipWith (appendArgWhenMultiple fps) fps
      <#> intercalate "\n\n"
      >>= (logEscape >>> liftEffect)

    appendArgWhenMultiple :: List FilePath -> String -> String -> String
    appendArgWhenMultiple fps a b = if length fps == 1
      then b -- don't do anything
      else a <> ":" <> "\n" <> b

-- | TODO: Some options are yet to be developed:
-- |
-- | -r, to reverse the result list
-- | -R, to recursively display the content of each dirs
-- | -S, to sort by size
-- | -i, with `inode` number included
-- | --version, not needed actually
options :: Parser LsOptions
options = ado
  withStats <- switch (
    short 'l' <>
    help "show stats"
  )
  withHiddenFiles <- switch (
    short 'a' <>
    help "show hidden files"
  )
  withTrailingSlash <- switch (
    short 'F' <>
    help "show trailing slash for directories"
  )
  in { withStats, withHiddenFiles, withTrailingSlash }

filePathsToShow :: Parser (List FilePath)
filePathsToShow = ado
  args <- many $ argument str (metavar "FILEPATH")
  in case args of
    Nil -> singleton "."
    _ -> args
