module PureShell.Ls.Ls where

import Prelude

import Data.Array (filter, last)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either, isRight)
import Data.Function.Uncurried (runFn0)
import Data.Maybe (maybe)
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.FS.Aff as FS
import Node.FS.Stats (Stats(..))
import Node.Path (FilePath)
import PureShell.Common.Utility (logEscape)

data LsError = FileOrDirNotExists

instance showLsError :: Show LsError where
  show _ = "No such file or directory"

-- | These options are actually coming from the parser
-- | `withStats`         -> to show basic statistic in a formatted way
-- | `withHiddenFiles`   -> to show hidden files under a directory, normally prefixed with `.`
-- | `withTrailingSlash` -> to show trailing slashes for directories
type LsOptions = {
  withStats :: Boolean,
  withHiddenFiles :: Boolean,
  withTrailingSlash :: Boolean
}

-- | TODO: Give better naming to it
type ErrorOrFileStats = (Either LsError (FilePath /\ Stats))

ls :: FilePath -> LsOptions -> Aff Unit
ls filePath options = do
  stats_ <- getStat filePath
  case stats_ of
    Left e -> liftEffect $ logShow e
    Right r@(_ /\ Stats s) ->
      if runFn0 s.isFile
      then showFileStats r
      else showDirStats filePath

-- | When the given input is a file, yeild the stat immediately
showFileStats :: (FilePath /\ Stats) -> Aff Unit
showFileStats = formatStats >>> log >>> liftEffect

-- | When the given input is a directory, list that directory with the stats
-- | then join the result with a breakline
showDirStats :: FilePath -> Aff Unit
showDirStats filePath = getDirs filePath >>= concludeStats >>> logEscape >>> liftEffect
  where
    concludeStats :: Array ErrorOrFileStats -> String
    concludeStats = filter isRight
      >>> map (either (const "oops") formatStats)
      >>> joinWith "\n"

getStat :: FilePath -> Aff ErrorOrFileStats
getStat filePath = do
  stats <- try $ FS.stat filePath
  pure $ bimap (const FileOrDirNotExists) (filePath /\ _) stats

getDirs :: FilePath -> Aff (Array ErrorOrFileStats)
getDirs dir = FS.readdir dir >>= traverse (appendDirPrefix >>> getStat)
  where
    appendDirPrefix content = dir <> "/" <> content

formatStats :: (FilePath /\ Stats) -> String
formatStats (filePath /\ (Stats stats)) =
  show stats.mode <> " " <>
  show stats.nlink <> " " <>
  show stats.uid <> " " <>
  show stats.gid <> " " <>
  show stats.size <> " " <>
  show stats.mtime <> " " <>
  maybe filePath identity trimmedDirName
  where
    trimmedDirName = split (Pattern "/") filePath # last
