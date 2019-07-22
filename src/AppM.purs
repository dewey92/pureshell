module PureShell.AppM
  ( AppM
  , runAppM
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Data.List (fromFoldable)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Node.FS.Aff as FS
import PureShell.Common.FileM (class MonadFs)

newtype AppM a = AppM (Aff a)

runAppM :: forall a. AppM a -> Aff a
runAppM (AppM a) = a

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
-- | Error handling
derive newtype instance monadThrowAppM :: MonadThrow Error AppM
derive newtype instance monadErrorAppM :: MonadError Error AppM
-- | Run under `Effect` and `Aff`
derive newtype instance monadEffAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

-- | Give `AppM` capability to abstract over file system
instance monadFsAppM :: MonadFs Error AppM where
  exists = AppM <<< FS.exists
  readTextFile e f = FS.readTextFile e f # AppM
  readDir = AppM <<< map fromFoldable <<< FS.readdir
  getStat = AppM <<< FS.stat
