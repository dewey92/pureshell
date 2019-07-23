module PureShell.Ls.Program where

import Prelude

import Data.List (List, intercalate, length, zipWith)
import Data.String (null)
import Data.Traversable (traverse)
import Effect.Class (liftEffect)
import Node.Path (FilePath)
import Options.Applicative (CommandFields, Mod, Parser, ReadM, argument, command, help, info, many, metavar, progDesc, short, switch)
import Options.Applicative.Types (readerAsk)
import PureShell.AppM (AppM)
import PureShell.Common.Utility (logEscape)
import PureShell.Ls.LsM (lsM)
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
    runMultipleFilePaths fps opts = traverse (\fp -> lsM fp opts) fps
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
filePathsToShow = many $ argument pickDir (metavar "FILEPATH")

pickDir :: ReadM String
pickDir = readerAsk >>= \s -> pure $ if null s then "." else s
