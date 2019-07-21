module PureShell.Ls.Program where

import Prelude

import Data.List (List)
import Data.String (null)
import Data.Traversable (traverse_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Path (FilePath)
import Options.Applicative (CommandFields, Mod, Parser, ReadM, argument, command, help, info, many, metavar, progDesc, short, switch)
import Options.Applicative.Types (readerAsk)
import PureShell.Common.Utility (logEscape)
import PureShell.Ls.Ls (LsOptions, ls)

program :: Mod CommandFields (Aff Unit)
program = command "ls" (info lsParser $ progDesc "List a directory")

lsParser :: Parser (Aff Unit)
lsParser = ado
  fps <- filePathsToShow
  opts <- options
  in runMultipleFilePaths fps opts
  where
    runMultipleFilePaths :: List FilePath -> LsOptions -> Aff Unit
    runMultipleFilePaths fps opts = traverse_ (\fileName -> ls fileName opts *> breakLine) fps
    breakLine :: Aff Unit
    breakLine = liftEffect $ logEscape "\n\n"

-- | TODO: Some options are yet to be developed
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
