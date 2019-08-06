module PureShell.Common.MonadPrompt where

import Prelude

class Monad m <= MonadPrompt m where
  prompt :: String -> m String
