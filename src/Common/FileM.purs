module PureShell.Common.FileM
  ( class MonadFs
  , exists
  , readTextFile
  , readDir
  , getStat
  , module ME
  ) where

import Control.Monad.Error.Class (try) as ME
import Control.Monad.Error.Class (class MonadError)
import Data.List (List)
import Node.Encoding (Encoding)
import Node.FS.Stats (Stats)
import Node.Path (FilePath)

-- | Define a monad for file system operations. All FS operations originally
-- | returning `Array` will be replaced by `List` for richer features. Please
-- | note also that I intentionally rename some functions from `Node.FS`
-- | just to make them nicer
class MonadError e m <= MonadFs e m | m -> e where
  -- checking
  exists :: FilePath -> m Boolean

  -- reading files or dirs
  readTextFile :: Encoding -> FilePath -> m String
  readDir :: FilePath -> m (List FilePath)
  getStat :: FilePath -> m Stats

-- | To be a monad, a type must implement the `Functor`, `Apply`, `Applicative`,
-- | and `Bind` type classes. Please implement also `MonadEffect` and `MonadAFf`
-- | later on to be able to run under `Effect` adn `Aff` monad.
-- |
-- | ```purescript
-- |    newtype AppM e a = AppM (Either e a)
--
-- |    derive newtype instance functorApp :: Functor (AppM e)
-- |    derive newtype instance applyApp :: Apply (AppM e)
-- |    derive newtype instance applicativeApp :: Applicative (AppM e)
-- |    derive newtype instance bindApp :: Bind (AppM e)
-- |    derive newtype instance monadApp :: Monad (AppM e)
-- |    derive newtype instance monadThrowApp :: MonadThrow e (AppM e)
-- |    derive newtype instance monadErrorApp :: MonadError e (AppM e)
-- |
-- |    instance appFs :: MonadFs e (AppM e) where
-- |      exists _ = pure true
-- |      readTextFile _ _ = pure ""
-- |      readDir _ = pure ("" : Nil)
-- |      getStat _ = return
-- | ```