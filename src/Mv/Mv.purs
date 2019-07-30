module PureShell.Mv.Mv where

import Prelude

import Node.Path (FilePath)
import PureShell.Common.MonadFS (class MonadFS)

mv :: ∀ m e. MonadFS e m => FilePath -> FilePath -> m (Unit)
mv _ _ = pure unit