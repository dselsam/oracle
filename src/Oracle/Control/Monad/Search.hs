{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

The Search Transformer: a monad transformer that adds non-deterministic choice.
Note that the entire search is conducted inside the monad it transforms.
-}

{-# LANGUAGE LambdaCase #-}
module Oracle.Control.Monad.Search where

import Oracle.Data.Embeddable

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Control.Monad.IO.Class
import Control.Monad.Trans (MonadTrans, lift)
import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad (ap, liftM, when, MonadPlus)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)

import Data.Vector (Vector)
import qualified Data.Vector as Vector

---------------------
-- MonadSearch
---------------------

class (Monad m) => MonadSearch m where
  deadend  :: String -> m a
  done     :: a -> m a
  choice'  :: Embeddable -> Vector (Embeddable, m a) -> m a

choice :: (MonadSearch m, HasToEmbeddable cp, HasToEmbeddable c) => cp -> [(c, m a)] -> m a
choice cp cs = choice' (toEmbeddable cp) $ fmap (\(c, psi) -> (toEmbeddable c, psi)) (Vector.fromList cs)

oneOf :: (MonadSearch m, HasToEmbeddable cp, HasToEmbeddable c) => cp -> [(c, a)] -> m a
oneOf cp cs = choice cp (fmap (\(c, x) -> (c, pure x)) cs)

oneOfSelf :: (MonadSearch m, HasToEmbeddable cp, HasToEmbeddable c) => cp -> [c] -> m c
oneOfSelf cp cs = oneOf cp (fmap (\c -> (c, c)) cs)

liftO :: (MonadSearch m) => Maybe a -> m a
liftO opt = case opt of
  Nothing -> deadend "liftO failed"
  Just x  -> pure x

instance (HasToEmbeddable s, MonadSearch m) => MonadSearch (StateT s m) where
  deadend       = lift . deadend
  done          = lift . done
  choice' cp cs = do
    s <- get
    let cs' = flip fmap cs $ \(c, f) -> (c, runStateT f s)
    (x, s) <- lift $ choice' cp cs'
    put s
    pure x

instance (HasToEmbeddable r, MonadSearch m) => MonadSearch (ReaderT r m) where
  deadend       = lift . deadend
  done          = lift . done
  choice' cp cs = do
    r <- ask
    let cs' = flip fmap cs $ \(c, f) -> (c, runReaderT f r)
    lift $ choice' cp cs'

---------------------
-- SearchT
---------------------

data SearchT m a = SearchT (m (Status m a))

data Status m a = Fail
                | Done a
                | Choice (ChoicePoint m a)

data ChoicePoint m a = ChoicePoint {
  snapshot :: Embeddable,
  choices  :: Vector (Embeddable, SearchT m a)
  }

instance (Monad m) => Functor (SearchT m) where
  fmap = liftM

instance (Monad m) => Monad (SearchT m) where
  SearchT f >>= g = SearchT $ do
    status <- f
    case status of
      Fail                       -> return $ Fail
      Done x                     -> let SearchT y = g x in y
      Choice (ChoicePoint cp cs) -> return $ Choice (ChoicePoint cp $ fmap (\(c, k) -> (c, k >>= g)) cs)
  return x = SearchT (return $ Done x)

instance (Monad m) => MonadSearch (SearchT m) where
  deadend _     = SearchT (pure Fail)
  done x        = SearchT (pure $ Done x)
  choice' cp cs = SearchT $ pure $ Choice $ ChoicePoint cp cs

instance (Monad m) => Applicative (SearchT m) where
  pure = return
  (<*>) = ap

instance (Monad m) => MonadPlus (SearchT m) where

instance (Monad m) => MonadFail (SearchT m) where
  fail = deadend

instance MonadTrans SearchT where
  lift f = SearchT (f >>= \x -> return (Done x))

instance (Monad m) => Alternative (SearchT m) where
  empty   = deadend "SearchT::Alternative::empty"
  f <|> g = error "why do we need alternative for guard?"

instance (Monad m, MonadIO m) => MonadIO (SearchT m) where
  liftIO a = lift . liftIO $ a
