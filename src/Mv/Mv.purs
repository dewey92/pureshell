module PureShell.Mv.Mv where

import Prelude

import Node.Path (FilePath)
import PureShell.Common.FileM (class MonadFs)

mv :: ∀ m e. MonadFs e m => FilePath -> FilePath -> m (Unit)
mv _ _ = pure unit