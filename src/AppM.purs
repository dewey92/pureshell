module PureShell.AppM
  ( AppM
  , runAppM
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Data.List (fromFoldable)
import Effect.Aff (Aff, Error, try)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import PureShell.Common.MonadFS (class MonadFS)

newtype AppM a = AppM (Aff a)

runAppM :: AppM ~> Aff
runAppM (AppM a) = a

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
-- | Run under `Effect` and `Aff`
derive newtype instance monadEffAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

safeLift :: forall a. Aff a -> ExceptT Error AppM a
safeLift = ExceptT <<< AppM <<< try

-- | Give `AppM` capability to abstract over file system
instance monadFSAppM :: MonadFS Error AppM where
  -- check
  exists = safeLift <<< FS.exists

  -- read
  readFile = safeLift <<< FS.readTextFile UTF8
  readDir = safeLift <<< map fromFoldable <<< FS.readdir
  getMetadata = safeLift <<< FS.stat
