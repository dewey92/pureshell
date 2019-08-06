module PureShell.AppM
  ( AppM
  , runAppM
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Data.Either (Either(..))
import Data.List (fromFoldable)
import Effect.Aff (Aff, Error, makeAff, nonCanceler, try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.ReadLine (close, createConsoleInterface, noCompletion, question)
import PureShell.Common.MonadFS (class MonadFS)
import PureShell.Common.MonadPrompt (class MonadPrompt)

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

-- |
-- | Give `AppM` capability to abstract over file system
-- |
instance monadFSAppM :: MonadFS Error AppM where
  -- check
  exists = safeLift <<< FS.exists
  -- read
  readFile = safeLift <<< FS.readTextFile UTF8
  readDir = safeLift <<< map fromFoldable <<< FS.readdir
  getMetadata = safeLift <<< FS.stat
  -- commands
  rename src dest = safeLift $ FS.rename src dest

-- |
-- | Give `AppM` capability to abstract over input prompts
-- |
instance monadPrompt :: MonadPrompt AppM where
  prompt = questionAff

-- | Implementation details for `AppM` by turning regular `question` which
-- | lives under `Effect` monad into `Aff a`  for easier chaining
questionAff :: String -> AppM String
questionAff q = do
  interface <- liftEffect $ createConsoleInterface noCompletion
  answer <- liftAff $ makeAff (go interface)
  liftEffect $ close interface -- close it to return to the "main screen"
  pure answer
  where
    go iface handler = question q (Right >>> handler) iface $> nonCanceler