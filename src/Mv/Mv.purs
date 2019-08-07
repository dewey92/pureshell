module PureShell.Mv.Mv where

import Prelude

import Control.Monad.Except (ExceptT(..), throwError, withExceptT)
import Data.Either (Either(..))
import Data.String (toLower)
import Node.Path (FilePath)
import PureShell.Common.MonadFS (class MonadFS, exists, rename)
import PureShell.Common.MonadPrompt (class MonadPrompt, prompt)

type MvOptions = {
  withForce :: Boolean,
  withPrompt :: Boolean,
  withVerboseMessage :: Boolean
}

data MvErrors
  = SrcNotExists
  | DestAlreadyExists
  | OperationAborted
  | MiscError

instance showMvErrors :: Show MvErrors where
  show SrcNotExists = "Please provide a valid source path"
  show DestAlreadyExists = "Destination filepath exists. Can't replace. Use -f to force"
  show OperationAborted = "Exited.."
  show MiscError = "Unknown error"

-- |
-- | The main program of `mv` command
-- |
mv :: ∀ m e
  .  MonadFS e m
  => MonadPrompt m
  => FilePath -> FilePath -> MvOptions -> ExceptT MvErrors m String
mv src dest opt = do
  srcExists <- withExceptT (const MiscError) (exists src)
  destExists <- withExceptT (const MiscError) (exists dest)

  -- prompt if necessary
  if not srcExists
    then throwError SrcNotExists
    else ExceptT (promptUser opt)

  -- force if necessary
  if destExists && not opt.withForce
    then throwError DestAlreadyExists
    else withExceptT (const MiscError) (rename src dest)

  -- when verbose option is active
  if opt.withVerboseMessage
    then pure $ "Renamed " <> src <> " to " <> dest
    else pure ""

-- |
-- | When `-v` option is set, prompt user to confirm their operation
-- |
promptUser :: ∀ m. MonadPrompt m => MvOptions -> m (Either MvErrors Unit)
promptUser opt = if not $ opt.withPrompt
  then pure $ Right unit -- skip prompting user
  else do
    answer <- prompt "Are you sure (y/n)? "
    if toLower answer == "y"
      then pure $ Right unit
      else pure $ Left OperationAborted
