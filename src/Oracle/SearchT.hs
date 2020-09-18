{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam
-}

module Oracle.SearchT where

import Oracle.Data.Embeddable

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Control.Monad.IO.Class
import Control.Monad.Trans (MonadTrans, lift)
import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad (ap, liftM, when, MonadPlus)

data SearchT m a = SearchT (m (Status m a))

data Status m a = Fail
                | Done a
                | Choice (ChoicePoint m a)

data ChoicePoint m a = ChoicePoint {
  snapshot :: Embeddable,
  choices  :: Seq (Embeddable, SearchT m a)
  }

deadend :: (Monad m) => String -> SearchT m a
deadend _ = SearchT (return Fail)

choice :: (Monad m, HasToEmbeddable cp, HasToEmbeddable c) => cp -> [(c, SearchT m a)] -> SearchT m a
choice cp cs = SearchT $ pure $ Choice $ ChoicePoint (toEmbeddable cp) $ fmap (\(c, psi) -> (toEmbeddable c, psi)) (Seq.fromList cs)

oneOf :: (Monad m, HasToEmbeddable cp, HasToEmbeddable c) => cp -> [c] -> SearchT m c
oneOf cp cs = SearchT $ pure $ Choice $ ChoicePoint (toEmbeddable cp) $ fmap (\c -> (toEmbeddable c, pure c)) (Seq.fromList cs)

-- Instances

instance (Monad m) => Monad (SearchT m) where
  SearchT f >>= g = SearchT $ do
    status <- f
    case status of
      Fail                       -> return $ Fail
      Done x                     -> let SearchT y = g x in y
      Choice (ChoicePoint cp cs) -> return $ Choice (ChoicePoint cp $ fmap (\(c, k) -> (c, k >>= g)) cs)
  return x = SearchT (return $ Done x)

instance (Monad m) => Functor (SearchT m) where
  fmap = liftM

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
